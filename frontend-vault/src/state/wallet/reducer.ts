import { createReducer } from "@reduxjs/toolkit"

import {
  updateMyVaultsAction,
  updateWalletAddressAction,
  updateBalanceAction,
} from "./actions"

export interface MyVaultInfo {
  id: number
  asset: string
  type: string
  locked: number
  collRatio: number
  debt: number
  risk: boolean

  //
  isStableCoin: boolean
  isLP: boolean
}

export interface WalletState {
  address: string
  balance: number
  myVaults: MyVaultInfo[]
}

const initialState: WalletState = {
  address: "",
  balance: 0,
  myVaults: [],
}

export default createReducer(initialState, (builder) =>
  builder
    .addCase(updateWalletAddressAction, (state, action) => {
      state.address = action.payload
    })
    .addCase(updateBalanceAction, (state, action) => {
      state.balance = action.payload
    })
    .addCase(updateMyVaultsAction, (state, action) => {
      state.myVaults = action.payload
    })
)
