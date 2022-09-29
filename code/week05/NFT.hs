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

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx = traceIfFalse "the given UTXO is not consumed" hasUTXO     &&
                          traceIfFalse "the minting amount is not 1"    onlyMintOne
    where
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        hasUTXO :: Bool
        hasUTXO = any (\txInInfo -> txInInfoOutRef txInInfo == oref) $ txInfoInputs txInfo

        onlyMintOne :: Bool
        onlyMintOne = case flattenValue (txInfoMint txInfo) of
                            [(_, tn', amount)] -> tn' == tn && amount == 1
                            _                  -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

------ off-chain code ------

data NFTParams = NFTParams {
    npToken :: !TokenName,
    npAddress   :: !Address
} deriving (Generic, ToJSON, FromJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt (npAddress np) -- find all utxos of the given address

    case Map.keys utxos of -- check if any UTXO exists
        []       -> Contract.logError @String "no utxo found" -- if there is no UTXO, log and close the contract
        oref : _ -> do -- if there's at least one utxo, use the first utxo as a parameter to the minting policy
            let tn      = npToken np
                val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
    where
        mint' = awaitPromise $ endpoint @"mint" mint

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints

    callEndpoint @"mint" h1 $ NFTParams {
        npToken     = "ABC",
        npAddress   = mockWalletAddress $ knownWallet 1
    }

    callEndpoint @"mint" h2 $ NFTParams {
        npToken     = "ABC",
        npAddress   = mockWalletAddress $ knownWallet 2
    }

    void $ Emulator.waitNSlots 2





