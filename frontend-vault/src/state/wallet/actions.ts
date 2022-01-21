import BigNumber from "bignumber.js"
import { createAction } from "@reduxjs/toolkit"

import { CardanoApi, MyVaultInfo } from "./types"

export const getCardanoApiAction = createAction<CardanoApi>(
  "wallet/getCardanoApiAction"
)

export const updateWalletAddressAction = createAction<string>(
  "wallet/updateWalletAddressAction"
)

export const updateBalancesAction = createAction<{ [key: string]: BigNumber }>(
  "wallet/updateBalancesAction"
)

export const updateMyVaultsAction = createAction<MyVaultInfo[]>(
  "wallet/updateMyVaultsAction"
)
