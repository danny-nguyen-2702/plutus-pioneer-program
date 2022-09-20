{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Week02.Gift where

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

-- The validator takes 3 arguments: Datum, Redeemer and Context. If there is no errors, then the validation passes. And if there's an error, then it fails. We don't care about the return value of this function, that's why it returns unit.
-- The INLINABLE pragma bellow allows the compiler to inline the definition of the mkValidator inside the Oxford brackets. Normally the Oxford brackets don't allow you to reference definitions outside of them, so normally you have to inline all your code into these brackets [|| ||]. That's why you need to add the INLINABLE pragma to your validator function and all helper functions that validator use. This is also a good indication that those functions are meant to be use in on-chain code, because everything that's supposed to be used in on-chain code needs this pragma.
{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

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