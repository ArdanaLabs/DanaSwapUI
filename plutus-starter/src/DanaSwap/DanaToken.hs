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

liqiduityTokenName, liquidityTokenSymbol :: String 
liqiduityTokenName = "DanaSwap"
liquidityTokenSymbol = "DSLP"

newtype DanaSwap = DanaSwap { owner :: PubKeyHash }

-- Converting from contract states as "spender". Redeemer is the Cardano term,
-- if I remember correctly.
-- | TODO: check proper numeric type. Probably not integer 
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
doTransfer :: TransferParams -> (PubKeyHash, Integer)
-- ^ TODO: Proper return when type is finally written

doTransfer TransferParams{..} = (tpTo, tpValue)
-- ^ TODO: Proper types && Logic-- end doTransfer
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
doTransferFrom :: TransferFromParams -> (PubKeyHash, PubKeyHash, Integer)
-- ^ TODO: Proper return when type is finally written

doTransferFrom TransferFromParams{..} = (tfpFrom, tfpTo, tfpValue)
-- ^ TODO: Proper types && Logic-- end doTransfer
-- sender.balance = balanceOf(from) - tpValue
-- receiver.balance = balanceOf(to) + tpValue
-- log transfer

balanceOf :: PubKeyHash -> Integer
-- ^ TODO: Proper types && Logic
balanceOf _ = 0

allowance :: PubKeyHash -> PubKeyHash -> Integer
-- ^ TODO: Proper types && Logic
allowance sender spender = 0

totalSupply :: Integer -> Integer
totalSupply _ = 0
-- ^ TODO: Proper types && Logic

minter :: PubKeyHash -> Integer
-- ^ TODO: Proper types && Logic
minter _ = 0

decimals :: Integer
-- ^ TODO: Proper types && Logic
decimals = 0

doApprove :: PubKeyHash -> Integer -> Bool 
-- ^ TODO: Proper types && Logic
doApprove spender value = True

doIncreaseAllowance :: PubKeyHash -> Integer -> Bool 
-- ^ TODO: Proper types && Logic
doIncreaseAllowance spender addedValue = True

doDeacreaseAllowance :: PubKeyHash -> Integer -> Bool 
-- ^ TODO: Proper types && Logic
doDeacreaseAllowance spender subtractedValue = True

doMint :: PubKeyHash -> Integer -> Bool 
-- ^ TODO: Proper types && Logic
doMint to value = True

doBurnFrom :: PubKeyHash -> Integer -> Bool
-- ^ TODO: Proper types && Logic
doBurnFrom to value = True

doSetMinter :: PubKeyHash -> Bool
-- ^ TODO: Proper types && Logic
doSetMinter address = True

doSetName :: PubKeyHash -> Bool
-- ^ TODO: Proper types && Logic
doSetName address = True

