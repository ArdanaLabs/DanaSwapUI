import { createReducer } from "@reduxjs/toolkit"

import {
  updateMyVaultsAction,
  updateWalletAddressAction,
  updateBalanceAction,
} from "./actions"
import { WalletState } from "./types"

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
