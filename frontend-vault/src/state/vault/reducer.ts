import { createReducer } from "@reduxjs/toolkit"
import { updateVaultsAction } from "./actions"

export interface VaultInfo {
  //  TBD
  id: number
  asset: string
  assetLogo: string
  type: string
  locked: number
  stabilityFee: number
  minCollRatio: number
}

export interface VaultState {
  vaults: VaultInfo[]
}

export const initialState: VaultState = {
  vaults: [],
}

export default createReducer(initialState, (builder) =>
  builder.addCase(updateVaultsAction, (state, action) => {
    state.vaults = action.payload
  })
)
