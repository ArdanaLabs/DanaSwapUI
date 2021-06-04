{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
module DanaSwap.Exchange where
import           Control.Monad             (void)
import qualified PlutusTx
import PlutusTx.Prelude
    ( Integral(div),
      Integer,
      AdditiveGroup((-)),
      AdditiveSemigroup((+)),
      MultiplicativeSemigroup((*)) )
import PlutusTx.Numeric ()
import           Ledger                     (Address, ValidatorCtx, scriptAddress)
import qualified Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts
import           Ledger.Value               (Value)
import Playground.Contract ()

-- TODO Nothing here should be considered final
-- This implementation started with v1 whitepaper as a basis,
-- but this is also a quick reference conversion from ERC20 contract
-- Specifically v2 of the protocol is the current target

{--|
    A single contract is associated with a token and 
    holds a liquidity pool of ADA and said token.

    Exchange rate is relative to liquidity pool size.
    The invariant is constant during trades but updated
    when the pool size is altered.

    invariant = adaPool * tokenPool
    updatedTokenPool = invariant / (adaPool - fee)
    updatedAdaPool = adaPool + adaAmount
    tokensOut = tokenPool - updatedTokenPool
-}
-- TODO proper numeric type
{-# INLINABLE getInvariant #-}
getInvariant :: Integer -> Integer -> Integer
getInvariant adaPoolAmount tokenPoolAmount = 
    adaPoolAmount * tokenPoolAmount

-- TODO proper numeric type
{-# INLINABLE getUpdatedTokenPoolAmount #-}
getUpdatedTokenPoolAmount :: Integer -> Integer -> Integer -> Integer
getUpdatedTokenPoolAmount adaPoolAmount tokenPoolAmount fee =
    let invariant = getInvariant adaPoolAmount tokenPoolAmount
        denominator = adaPoolAmount - fee
    in invariant `div` denominator

-- TODO proper numeric type
{-# INLINABLE getUpdatedAdaPoolAmount #-}
getUpdatedAdaPoolAmount :: Integer -> Integer -> Integer
getUpdatedAdaPoolAmount adaPoolAmount adaAmount = 
    adaPoolAmount + adaAmount

-- TODO proper numeric type
{-# INLINABLE getTokensOutAmount #-}
getTokensOutAmount :: Integer -> Integer ->  Integer -> Integer
getTokensOutAmount adaPoolAmount tokenPoolAmount fee =
    let updatedTokenPoolAmount = getUpdatedTokenPoolAmount adaPoolAmount tokenPoolAmount fee
    in tokenPoolAmount - updatedTokenPoolAmount

-- | Swap

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doAdaToTokenSwap #-}
doAdaToTokenSwap :: Address -> Address 
doAdaToTokenSwap senderAddress = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToAdaSwap #-}
doTokenToAdaSwap :: Address -> Address 
doTokenToAdaSwap senderAddress = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToTokenSwap #-}
doTokenToTokenSwap :: Address -> Address 
doTokenToTokenSwap senderAddress = senderAddress

-- | Transfer

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doAdaToTokenTransfer #-}
doAdaToTokenTransfer :: Address -> Address 
doAdaToTokenTransfer senderAddress = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToAdaTransfer #-}
doTokenToAdaTransfer :: Address -> Address 
doTokenToAdaTransfer senderAddress = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToTokenTransfer #-}
doTokenToTokenTransfer :: Address -> Address 
doTokenToTokenTransfer senderAddress = senderAddress

-- | Liquidity

-- TODO: Liquidity Token

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE addLiquidity #-}
addLiquidity :: Address -> Address
addLiquidity senderAddress  = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE removeLiquidity #-}
removeLiquidity :: Address -> Address
removeLiquidity senderAddress  = senderAddress

-- | Exchange

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToExchangeTransfer #-}
doTokenToExchangeTransfer :: Address -> Address
doTokenToExchangeTransfer senderAddress  = senderAddress

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE doTokenToExchangeSwap #-}
doTokenToExchangeSwap :: Address -> Address
doTokenToExchangeSwap senderAddress  = senderAddress