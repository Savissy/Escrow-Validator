{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, putStrLn, String, FilePath, (<>))
import qualified Prelude as P

-- Core on-chain API (V2)
import Plutus.V2.Ledger.Api

-- helper modules containing tx/value utilities
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, scriptContextTxInfo)
import Plutus.V1.Ledger.Value    (valueOf, adaSymbol, adaToken)

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified Data.ByteString.Lazy  as LBS
import Codec.Serialise (serialise)

------------------------------------------------------------------------
-- Datum, Redeemer
------------------------------------------------------------------------

-- | Escrow datum carries buyer, seller and amount (all on-chain)
data EscrowDatum = EscrowDatum
    { edBuyer  :: PubKeyHash
    , edSeller :: PubKeyHash
    , edAmount :: Integer    -- lovelace
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

-- | Redeemer actions
data EscrowAction = PaySeller | RefundBuyer
PlutusTx.unstableMakeIsData ''EscrowAction

------------------------------------------------------------------------
-- Validator logic (datum carries buyer/seller -> no makeLift needed)
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
         traceIfFalse "Buyer signature missing" signedByBuyer &&
         traceIfFalse "Seller not paid expected amount" sellerPaid
      RefundBuyer ->
         traceIfFalse "Seller signature missing" signedBySeller &&
         traceIfFalse "Buyer not refunded" buyerPaid
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBuyer :: Bool
    signedByBuyer = txSignedBy info (edBuyer dat)

    signedBySeller :: Bool
    signedBySeller = txSignedBy info (edSeller dat)

    -- use valuePaidTo (provided by Plutus.V2.Ledger.Contexts) to sum outputs paid to a PubKeyHash
    sellerPaid :: Bool
    sellerPaid =
        let v = valuePaidTo info (edSeller dat)
         in valueOf v adaSymbol adaToken >= edAmount dat

    buyerPaid :: Bool
    buyerPaid =
        let v = valuePaidTo info (edBuyer dat)
         in valueOf v adaSymbol adaToken >= edAmount dat

------------------------------------------------------------------------
-- Untyped wrapper and compile
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r ctx =
    let datum     = unsafeFromBuiltinData @EscrowDatum d
        redeemer  = unsafeFromBuiltinData @EscrowAction r
        ctx'      = unsafeFromBuiltinData @ScriptContext ctx
    in if mkValidator datum redeemer ctx' then () else error ()

validator :: Validator
validator = mkValidatorScript $ $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Write validator to file
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    -- serialise the on-chain script (CBOR)
    LBS.writeFile path (serialise val)
    putStrLn $ "Validator written to " <> path

------------------------------------------------------------------------
-- Main: writes validator.plutus
------------------------------------------------------------------------

main :: IO ()
main = do
    -- This validator has no compile-time params.
    -- buyer/seller/amount are supplied when creating the UTxO (Datum).
    writeValidator "validator.plutus" validator
    putStrLn "Escrow validator (datum-parameterized) generated."
