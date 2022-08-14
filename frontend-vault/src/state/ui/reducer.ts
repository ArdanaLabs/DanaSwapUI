import { createReducer } from "@reduxjs/toolkit"
import { toggleUiModalAction } from "./actions"

export interface ModalUIState {
  open: boolean
  type?: string
}

export interface UIState {
  modal: ModalUIState
}

export const modalInitialState: ModalUIState = {
  open: false,
  type: "",
}

export const initialState: UIState = {
  modal: modalInitialState,
}

export default createReducer(initialState, (builder) =>
  builder.addCase(toggleUiModalAction, (state, action) => {
    state.modal = { ...state.modal, ...action.payload }
  })
)
