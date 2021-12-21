import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import { toggleUiModalAction } from "./actions"
import { UiState } from "./reducer"

export function useUiModal(): any {
  const dispatch = useDispatch<AppDispatch>()

  const { modal } = useSelector<AppState, UiState>(
    (state) => state.ui,
    shallowEqual
  )

  const toggleModal = useCallback(
    (info) => {
      dispatch(toggleUiModalAction(info))
    },
    [dispatch]
  )

  return {
    modal,
    toggleModal,
  }
}
