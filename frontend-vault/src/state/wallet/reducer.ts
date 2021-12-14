import { createReducer } from "@reduxjs/toolkit"
import { updateWalletAddressAction } from "./actions"

export interface WalletState {
  address: string
}

export const initialState: WalletState = {
  address: "",
}

export default createReducer(initialState, (builder) =>
  builder.addCase(updateWalletAddressAction, (state, action) => {
    state.address = action.payload.address
  })
)
