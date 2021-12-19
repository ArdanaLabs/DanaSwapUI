import { createAction } from "@reduxjs/toolkit"
import { VaultInfo } from "./reducer"

export const updateWalletAddressAction = createAction<{ address: string }>(
  "wallet/updateWalletAddressAction"
)

export const updateMyVaultsAction = createAction<{ vaults: VaultInfo[] }>(
  "wallet/updateMyVaultsAction"
)
