import { createAction } from "@reduxjs/toolkit"
import { VaultInfo } from "./types"

export const updateVaultsAction = createAction<VaultInfo[]>(
  "vault/updateVaultsAction"
)
