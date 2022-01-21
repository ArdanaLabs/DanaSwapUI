import { createReducer } from "@reduxjs/toolkit"

import {
  getCardanoApiAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
  updateBalancesAction,
} from "./actions"
import { WalletState } from "./types"

const initialState: WalletState = {
  cardanoApi: null,
  walletType: null,
  address: "",
  balances: {},
  myVaults: [],
}

export default createReducer(initialState, (builder) =>
  builder
    .addCase(getCardanoApiAction, (state, action) => {
      state.cardanoApi = action.payload
    })
    .addCase(updateWalletAddressAction, (state, action) => {
      state.address = action.payload
    })
    .addCase(updateBalancesAction, (state, action) => {
      state.balances = { ...state.balances, ...action.payload }
    })
    .addCase(updateMyVaultsAction, (state, action) => {
      state.myVaults = action.payload
    })
)
