import { createAction } from "@reduxjs/toolkit"

export const toggleUiModalAction = createAction<{
  open: boolean
  type?: string
}>("ui/toggleUiModalAction")
