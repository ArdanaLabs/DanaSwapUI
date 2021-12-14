import { createAction } from "@reduxjs/toolkit"

export const updateWalletAddressAction = createAction<{ address: string }>(
  "wallet/updateWalletAddressAction"
)
