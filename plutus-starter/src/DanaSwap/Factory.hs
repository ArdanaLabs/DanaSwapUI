{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DanaSwap.Factory where
import qualified PlutusTx
import PlutusTx.Prelude ()
import qualified Prelude
import           Ledger                   (Address)

-- TODO Nothing here should be considered final
-- This implementation started with v1 whitepaper as a basis,
-- but this is also a quick reference conversion from ERC20 contract
-- Specifically v2 of the protocol is the current target

-- feeTo
-- feeToSetter
-- public getPair
-- public allPairs
-- Event PairCreated
-- public allPairsLength
-- public createPair
-- public setFeeTo
-- public setFeeToSetter

{--|
    A factory and registry for exchanges.

    One-exchange-per-token limit is the only check the factory performs.
    
    `createExchange` deploys a contract for any token without one.

    Stored in the factory are tokens, and their associated exchanges.

    `getExchange` returns exchange address of token address.

    `getToken` returns the token address of exchange address.
-}
-- TODO: look into difference between Ledger Address and PubKeyHash
type ExchangeAddress = Address 
type TokenAddress = Address

-- TODO: look into difference between Ledger Address and PubKeyHash
-- TODO: logic body
{-# INLINABLE createExchange #-}
createExchange :: TokenAddress -> ExchangeAddress
createExchange tokenAddress = tokenAddress -- TODO
-- Check that the exchange address doesn't exist
-- Create a new exchange with the provided template
-- tokenToExchange = newExchangeAddress
-- exchangeToToken = tokenAddress
-- return newExchangeAddress

{-# INLINABLE getExchange #-}
getExchange :: TokenAddress -> ExchangeAddress
getExchange a = a -- TODO

{-# INLINABLE getToken #-}
getToken :: ExchangeAddress -> TokenAddress
getToken a = a -- TODO