import { createReducer } from "@reduxjs/toolkit"
import BigNumber from "bignumber.js"

import {
  getCardanoApiAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
  updateBalanceAction,
} from "./actions"
import { WalletState } from "./types"

const initialState: WalletState = {
  cardanoApi: null,
  walletType: null,
  address: "",
  balance: new BigNumber(0),
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
    .addCase(updateBalanceAction, (state, action) => {
      state.balance = action.payload
    })
    .addCase(updateMyVaultsAction, (state, action) => {
      state.myVaults = action.payload
    })
)
