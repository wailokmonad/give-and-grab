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
import           Plutus.Contract      as Contract
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value         as Value
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)


data VestingDatum = VestingDatum
    { amount :: !Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE extractValue #-}
extractValue :: Maybe Datum -> Maybe VestingDatum
extractValue md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = True

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

data GiveParams = GiveParams
    { gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    let _amount = gpAmount gp
    if _amount > 8000000
        then logInfo @String "amount too big"
        else do
            let dat = VestingDatum { amount = _amount }
                tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ _amount
            ledgerTx <- submitTxConstraints typedValidator tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "give amount"
            logInfo @Integer $ _amount
    
    
grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress
    x     <- findGive
    let totalAmount = sum' x
    logInfo @String $ "Total amount"
    logInfo @Integer $ totalAmount
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let dat = VestingDatum
                        { amount = PlutusTx.Prelude.divide totalAmount 2
                        }
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedValidator 
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          ( Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ amount dat ) 
            ledgerTx <- submitTxConstraintsWith @Vesting lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"


findGive :: forall w s e. AsContractError e => Contract w s e [(TxOutRef, ChainIndexTxOut,VestingDatum, Integer)]
findGive = do
    utxos <- utxosAt scrAddress
    return $ PlutusTx.Prelude.mapMaybe g $ Map.toList utxos
  where 
    g :: (TxOutRef, ChainIndexTxOut) -> Maybe (TxOutRef, ChainIndexTxOut, VestingDatum, Integer)
    g (oref, o) = do
        dat <- extractValue $ either (const Nothing) Just $ _ciTxOutDatum o 
        return (oref, o, dat, amount dat) 

sum' :: [(TxOutRef, ChainIndexTxOut, VestingDatum, Integer)] -> Integer  
sum' xs =  PlutusTx.Prelude.foldl (\acc (a,b,c,d) -> acc + d) 0 xs  


endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
