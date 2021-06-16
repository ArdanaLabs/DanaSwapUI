{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DanaSwap.DanaToken where
import           Control.Monad             (void)
import           Data.Aeson                (FromJSON, ToJSON)
import           GHC.Generics              (Generic)
import           Plutus.Contract
import qualified PlutusTx         as PlutusTx
import           PlutusTx.Prelude
import           Ledger
import qualified Ledger.Ada                as Ada
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Schema
import           Wallet.Emulator.Wallet

-- Adapted from Vyper contract - Draft

liqiduityTokenName, liquidityTokenSymbol :: String 
liqiduityTokenName = "DanaSwap"
liquidityTokenSymbol = "DSLP"
-- ^ TODO: set name, symbol, and minter on deploy/init

newtype DanaSwap = DanaSwap { owner :: PubKeyHash }

-- | TODO: check proper numeric type. Probably not integer 
-- not yet in use
data ApprovalParams = ApprovalParams { 
    apOwner :: PubKeyHash
    , apSpender :: PubKeyHash
    , apValue :: Integer 
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | TODO: check proper numeric type. Probably not integer 
data TransferParams = TransferParams { 
    tpTo :: PubKeyHash
    , tpValue :: Integer 
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- TODO: ContractEvent
-- | TODO: check proper numeric type. Probably not integer 
doTransfer :: TransferParams -> Bool
doTransfer TransferParams{..} = True -- TODO
-- sender.balance = balanceOf(from) - tpValue
-- receiver.balance = balanceOf(to) + tpValue
-- log transfer

-- | TODO: check proper numeric type. Probably not integer 
data TransferFromParams = TransferFromParams { 
    tfpFrom :: PubKeyHash
    , tfpTo :: PubKeyHash
    , tfpValue :: Integer 
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- TODO: ContractEvent
-- | TODO: check proper numeric type. Probably not integer
{-# INLINABLE doTransferFrom #-}
doTransferFrom :: TransferFromParams -> Bool
doTransferFrom TransferFromParams{..} = True -- TODO
-- sender.balance = balanceOf(from) - tpValue
-- receiver.balance = balanceOf(to) + tpValue
-- log transfer

{-
    Approve `spender` to transfer amount for sender
-}
{-# INLINABLE doApprove #-}
doApprove :: PubKeyHash -> Integer -> Bool 
doApprove spender value = True -- TODO

{-
    Alternative to `approve`
-}
{-# INLINABLE doIncreaseAllowance #-}
doIncreaseAllowance :: PubKeyHash -> Integer -> Bool 
doIncreaseAllowance spender subtractedValue = True -- TODO
-- look into this and possible race condition via vyper contract line 119

{-
    Alternative to `approve`
-}
{-# INLINABLE doDecreaseAllowance #-}
doDecreaseAllowance :: PubKeyHash -> Integer -> Bool 
doDecreaseAllowance spender subtractedValue = True -- TODO
-- look into this and possible race condition via vyper contract line 135

{-
    Mint token amount to address if sender is minter
-}
{-# INLINABLE doMint #-}
doMint :: PubKeyHash -> Integer -> Bool 
doMint to value = True -- TODO
-- totalSupply += ...
-- toBalance += ...

{-
    Burn amount from address if sender is minter
-}
{-# INLINABLE doBurnFrom #-}
doBurnFrom :: PubKeyHash -> Integer -> Bool
doBurnFrom to value = True -- TODO
-- totalSupply -= ...
-- toBalance = ...

{-| 
    Set minter address
-}
{-# INLINABLE setMinter #-}
setMinter :: PubKeyHash -> Bool
setMinter minter = True -- TODO
-- if sender address is minter address => line 184 of vyper contract

{-| 
    Set name and symbol if the sender is the owner address of the minter.
-}
{-# INLINABLE setName #-}
setName :: String -> String -> Bool
setName name symbol = True -- TODO
-- if owner of the minter is the sender
-- set name

getBalanceOf :: PubKeyHash -> Integer
getBalanceOf _ = 0 -- TODO

setAllowance :: PubKeyHash -> PubKeyHash -> Integer
setAllowance sender spender = 0 -- TODO

getTotalSupply :: Integer -> Integer
getTotalSupply _ = 0 -- TODO

getDecimals :: () -> Integer
getDecimals () = 18