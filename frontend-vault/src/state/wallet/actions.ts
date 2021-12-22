import { createAction } from "@reduxjs/toolkit"

import { MyVaultInfo } from "./reducer"

export const updateWalletAddressAction = createAction<string>(
  "wallet/updateWalletAddressAction"
)

export const updateBalanceAction = createAction<number>(
  "wallet/updateBalanceAction"
)

export const updateMyVaultsAction = createAction<MyVaultInfo[]>(
  "wallet/updateMyVaultsAction"
)
