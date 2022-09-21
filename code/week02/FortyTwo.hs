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

module Week02.FortyTwo where

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
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Now we write a validator that do care about the redeemer. The idea is that users can only spend the UTXOs in the script address when the spending transaction provide the Redeemer equal to 42
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ r _
    | r == Builtins.mkI 42  = ()
    | otherwise             = traceError "Redeemer is not 42, validation fails."

-- takes the Haskell function (that gives the business logic of validation) and turns it into an actual validator by compiling into Plutus Core script
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-- [|| mkValidator ||] : use the Oxford brackets to convert the Haskell function into a syntactical representation of this function
-- PlutusTx.compile takes the syntactical representation of a Haskell function and turns it into syntactical representation of the corresponding Plutus Core function
-- $$ takes the syntactical representation of Plutus Core function and splices into the source code

-- get the hash of the validator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator


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
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount -- construct the transaction that send the amount of lovelace to the Script Address

    ledgerTx <- submitTx tx -- submit the transaction

    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx -- wait for the confirmation

    logInfo @String $ printf "made a gift of %d lovelace" amount



-- grab function takes all UTXOs of the Script Address and send it to user. The spending fund transaction doesn't need to include the Datum, but needs to provide the Redeemer and the Validator code
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab n = do
    utxos <- utxosAt scrAddress -- looks up for all UTXOs sitting at this script address
    let orefs = fst <$> Map.toList utxos -- get all references of these UTXOs
        lookups =   Constraints.unspentOutputs utxos <> -- tell the wallet where to find all those UTXOs
                    Constraints.otherScript validator -- tell the wallet about the actual validator (if you want to consume an UTXO sitting at a script address, the spending transaction needs to provide the actual validator code)
        tx :: TxConstraints Void Void
        tx = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI n | oref <- orefs] -- construct the transaction that spends all UTXOs sitting at the Script Address

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