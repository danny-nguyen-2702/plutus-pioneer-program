{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Parameterized where

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
data VestingParam = VestingParam {
    beneficiary :: PaymentPubKeyHash,
    deadline :: POSIXTime
} deriving Show

-- making the data type VestingParam an instance of the Lift class
PlutusTx.makeLift ''VestingParam

-- now we get the beneficiary and deadline from the additional param, not by the datum anymore
{-# INLINABLE mkValidator #-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator param () () ctx =   traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                                traceIfFalse "deadline not reached" deadlineReached
    where
        -- get the transaction info from the context
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- check if the signer of this spending transaction is the beneficiary
        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary param

        -- check if the deadline has passed
        deadlineReached :: Bool
        deadlineReached = contains (from $ deadline param) $ txInfoValidRange info


data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

-- change constant typed validator to typed validator that takes parameter
-- and use liftCode function to the parameter so that the Plutus compiler can compile that parameter into Plutus Core. When we apply the lifeCode function to a given data, we get back the Plutus Core program that corresponding to the data.
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p) -- use applyCode to apply the Plutus Core version of mkValidator to the Plutus Core version of p parameter so we get back the Plutus Core version of a validator that takes only 3 arguments Datum, Redeemer and Context
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()


validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator


valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator


scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator



----------------- off-chain code -----------------

data GiveParams = GiveParams {
    gpBeneficiary   :: !PaymentPubKeyHash,
    gpDeadline      :: !POSIXTime,
    gpAmount        :: !Integer
} deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema = 
        Endpoint "give" GiveParams
    .\/ Endpoint "grab" POSIXTime -- now we need to know the deadline in the grab endpoint because this data is no longer store in the output's datum


-- give some fund to a beneficiary at specific deadline
give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    -- takes the beneficiary, deadline and amount data, then construct the transaction with that param
    let p = VestingParam {
        beneficiary     = gpBeneficiary gp,
        deadline        = gpDeadline gp
    }
        tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)


-- the beneficiary provide the deadline for the grab endpoint and the wallet will try to find the script address corrensponding to the beneficiary and the deadline
grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab deadline = do
    now         <- currentTime
    pubkeyhash  <- ownPaymentPubKeyHash

    if now < deadline
        then logInfo @String $ "Too early"
        else do
            let p = VestingParam {
                beneficiary = pubkeyhash,
                deadline = deadline
            }

            utxos <- utxosAt $ scrAddress p -- get all the utxos sitting at the script address that corresponding to the provided deadline and signer. Because now all the gift UTXOs sitting at different addresses, we need to provide the VestingParam to the scrAddress function to find the right script address
            
            if Map.null utxos
                then logInfo @String "no gifts available"
                else do
                    let orefs = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos <>
                                  Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                             Constraints.mustValidateIn (from now)

                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "collected gifts"


endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
    where
        give' = endpoint @"give" give
        grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []