import { createReducer } from "@reduxjs/toolkit"
import { updateMyVaultsAction, updateWalletAddressAction } from "./actions"

export interface VaultInfo {
  //  TBD
  name: string
  image: string
  locked: number
  debt: number
  usdRate: number
  risk: boolean
}
export interface WalletState {
  address: string
  balance: number
  myVaults: VaultInfo[]
}

export const initialState: WalletState = {
  address: "",
  balance: 0.32,
  myVaults: [],
}

export default createReducer(initialState, (builder) =>
  builder
    .addCase(updateWalletAddressAction, (state, action) => {
      state.address = action.payload.address
    })
    .addCase(updateMyVaultsAction, (state, action) => {
      state.myVaults = action.payload.vaults
    })
)
