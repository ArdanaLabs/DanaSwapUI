import { createAction } from "@reduxjs/toolkit"
import { VaultInfo } from "./reducer"

export const updateVaultsAction = createAction<VaultInfo[]>(
  "vault/updateVaultsAction"
)
