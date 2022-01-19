import { createReducer } from "@reduxjs/toolkit"
import { updateVaultsAction } from "./actions"
import { VaultState } from "./types"

export const initialState: VaultState = {
  vaults: [],
}

export default createReducer(initialState, (builder) =>
  builder.addCase(updateVaultsAction, (state, action) => {
    state.vaults = action.payload
  })
)
