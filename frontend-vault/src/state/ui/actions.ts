import { createAction } from "@reduxjs/toolkit"
import { ModalUIState } from "./reducer"

export const toggleUiModalAction = createAction<ModalUIState>(
  "ui/toggleUiModalAction"
)
