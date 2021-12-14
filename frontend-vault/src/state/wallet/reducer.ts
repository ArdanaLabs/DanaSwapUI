import { createReducer } from "@reduxjs/toolkit"
import { updateWalletAddressAction } from "./actions"

export interface WalletState {
  address: string
  balance: number
}

export const initialState: WalletState = {
  address: "",
  balance: 0.32,
}

export default createReducer(initialState, (builder) =>
  builder.addCase(updateWalletAddressAction, (state, action) => {
    state.address = action.payload.address
  })
)
