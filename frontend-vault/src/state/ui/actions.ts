import { createAction } from "@reduxjs/toolkit"

export const toggleUiModalAction = createAction<{
  open: boolean
  asset?: string
}>("ui/toggleUiModalAction")
