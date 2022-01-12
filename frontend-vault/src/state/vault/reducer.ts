import { createReducer } from "@reduxjs/toolkit"
// import { FilterType } from "components/DataGrid/TokenAssetGridFilter"
import { updateVaultsAction } from "./actions"

export interface VaultInfo {
  //  TBD
  id: number
  asset: string
  type: string
  locked: number
  stabilityFee: number
  liquidationFee: number
  minCollRatio: number

  dustLimit: number
  // category: FilterType
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
