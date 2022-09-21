{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-} -- the standard Haskell Prelude doesn't not get automatically imported
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Week02.IsData where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Typed.Scripts      as Scripts -- using the typed version of the scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


newtype MySillyRedeemer = MySillyRedeemer Integer

PlutusTx.unstableMakeIsData ''MySillyRedeemer -- write the instances of the MySillyRedeemer for the IsData class


-- Now we write a validator that takes custom type as arguments. It's more convenient if you have strong types (more restricted types) that much better suited to your business logic instead of general BuiltinData type. But in production, you should you the BuiltinData type to maximize the performance. When you use custom types for your validator arguments, the compiled Plutus Core script becomes bigger and the number of execution steps needed to execute the validator and the memory comsumption they go up.
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator _ (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" (r == 42)

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = MySillyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) 
    where
        wrap = Scripts.wrapValidator @() @MySillyRedeemer -- convert the validator arguments from BuiltinData type to the custom typed. The first, second and third argument will be converted to (), Integer and ScriptContext. The convertion logic will be handle by instances of 2 classes ToData and FromData which defined in PlutusTx module

-- get the un-typed validator
validator :: Validator
validator = Scripts.validatorScript typedValidator

-- [|| mkValidator ||] : use the Oxford brackets to convert the Haskell function into a syntactical representation of this function
-- PlutusTx.compile takes the syntactical representation of a Haskell function and turns it into syntactical representation of the corresponding Plutus Core function
-- $$ takes the syntactical representation of Plutus Core function and splices into the source code

-- get the hash of the validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator


-- get address of that script on the blockchain
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


---------------------- off-chain code ----------------------

-- Endpoints are ways for the user to trigger something and enter data. For this Gift smartcontract, we will have 2 endpoints for users. User can send money to the Script Address via the 'give' endpoint and user can take money from the Script Address by the 'grab' endpoint.
type GiftSchema =
        Endpoint "give" Integer
    .\/ Endpoint "grab" Integer


-- give function taking the integer amount of lovelace and create a transaction that send this amount of lovelace to the Script Address. The providing fund transaction doesn't need to include the Validator code, but needs to provide the Datum
give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount -- construct the transaction that send the amount of lovelace to the Script Address

    ledgerTx <- submitTxConstraints typedValidator tx -- submit the transaction

    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- wait for the confirmation

    logInfo @String $ printf "made a gift of %d lovelace" amount



-- grab function takes all UTXOs of the Script Address and send it to user. The spending fund transaction doesn't need to include the Datum, but needs to provide the Redeemer and the Validator code
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress -- looks up for all UTXOs sitting at this script address
    let orefs = fst <$> Map.toList utxos -- get all references of these UTXOs
        lookups =   Constraints.unspentOutputs utxos <> -- tell the wallet where to find all those UTXOs
                    Constraints.otherScript validator -- tell the wallet about the actual validator (if you want to consume an UTXO sitting at a script address, the spending transaction needs to provide the actual validator code)
        tx :: TxConstraints Void Void
        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MySillyRedeemer r) | oref <- orefs] -- construct the transaction that spends all UTXOs sitting at the Script Address

    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ "collected gifts"


endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints -- the select function offers users choices between the give' and the grab', and then we loop over it again and again recursively
    where
        give' = endpoint @"give" give
        grab' = endpoint @"grab" grab


mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies [] -- makes the ADA currency available in the plutus playground