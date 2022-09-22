{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

-- each UTXOs sitting at the script address has a datum that specify the beneficiary of that UTXO and the deadline he/she could get access to that UTXO
data VestingDatum = VestingDatum {
    beneficiary :: PaymentPubKeyHash,
    deadline :: POSIXTime
} deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- for the validator logic, we only care about the datum and the transaction context, we don't need to use redeemer in the spending transaction
-- we get the beneficiary and the deadline from the datum, then check with the signer and the valid range of the transaction. If the signer of the transaction is the beneficiary and the deadline has been reached, the validation returns True and the signer could unlock the fund 
{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator datum () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                           traceIfFalse "deadline not reached" deadlineReached
    where
        -- get the transaction info from the context
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- check if the signer of this spending transaction is the beneficiary
        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary datum

        -- check if the deadline has passed
        deadlineReached :: Bool
        deadlineReached = contains (from $ deadline datum) $ txInfoValidRange info


data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()


typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @VestingDatum @()


validator :: Validator
validator = Scripts.validatorScript typedValidator


valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator


scrAddress :: Ledger.Address
scrAddress = scriptAddress validator



----------------- off-chain code -----------------

data GiveParams = GiveParams {
    gpBeneficiary   :: !PaymentPubKeyHash,
    gpDeadline      :: !POSIXTime,
    gpAmount        :: !Integer
} deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema = 
        Endpoint "give" GiveParams
    .\/ Endpoint "grab" ()


-- give some fund to a beneficiary at specific deadline
give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    -- takes the beneficiary, deadline and amount data, then construct the transaction with that datum
    let datum = VestingDatum {
        beneficiary     = gpBeneficiary gp,
        deadline        = gpDeadline gp
    }
        tx = Constraints.mustPayToTheScript datum $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)


-- the beneficiary try to get the funds from the script address
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now         <- currentTime
    pubkeyhash  <- ownPaymentPubKeyHash
    utxos       <- Map.filter (isSuitable pubkeyhash now) <$> utxosAt scrAddress -- get all the utxos sitting at the scrAddress that belong to the signer of spending transaction and have reached deadline 

    if Map.null utxos
        then logInfo @String "no gifts available"
        else do
            let orefs = fst <$> Map.toList utxos
                lookups =   Constraints.unspentOutputs utxos <>
                            Constraints.otherScript validator
                tx :: TxConstraints Void Void
                tx =    mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                        Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
    where
        -- check datum of an utxo to see if the beneficiary and deadline of that datum match with the signer and time of transaction
        isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
        isSuitable pkh now output = case _ciTxOutDatum output of
            Left _              -> False
            Right (Datum e)     -> case PlutusTx.fromBuiltinData e of
                Nothing     -> False
                Just d      -> beneficiary d == pkh && deadline d <= now


endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" give
        grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []