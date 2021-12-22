import { createReducer } from "@reduxjs/toolkit"
import { toggleUiModalAction } from "./actions"

export interface UiState {
  modal: {
    open: boolean
    type?: string
  }
}

export const initialState: UiState = {
  modal: {
    open: false,
    type: "",
  },
}

export default createReducer(initialState, (builder) =>
  builder.addCase(toggleUiModalAction, (state, action) => {
    state.modal = { ...state.modal, ...action.payload }
  })
)
