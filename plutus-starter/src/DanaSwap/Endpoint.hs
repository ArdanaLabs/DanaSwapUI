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
module DanaSwap.Endpoint where
import           Plutus.Contract  hiding (when)
import           PlutusTx.Prelude
import           Playground.Contract

type Erc20Schema =
    BlockchainActions
    .\/ Endpoint "approve" ()
    .\/ Endpoint "transfer" ()
    .\/ Endpoint "transferFrom" ()
    .\/ Endpoint "permit" ()

type FactorySchema =
    BlockchainActions
    .\/ Endpoint "allPairsLength" ()
    .\/ Endpoint "createPair" ()
    .\/ Endpoint "setFeeTo" ()
    .\/ Endpoint "setFeeToSetter" ()

type DanaSwapSchema =
    BlockchainActions
    .\/ Endpoint "initialize" ()
    .\/ Endpoint "mint" ()
    .\/ Endpoint "burn" ()
    .\/ Endpoint "swap" ()
    .\/ Endpoint "skim" ()
    .\/ Endpoint "sync" ()