import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import { toggleUiModalAction } from "./actions"
import { ModalUIState, UIState } from "./reducer"

export function useUiModal(): {
  modal: ModalUIState
  toggleModal: (info: ModalUIState) => void
} {
  const dispatch = useDispatch<AppDispatch>()

  const { modal } = useSelector<AppState, UIState>(
    (state) => state.ui,
    shallowEqual
  )

  const toggleModal = (info: ModalUIState): void => {
    dispatch(toggleUiModalAction(info))
  }

  return {
    modal,
    toggleModal,
  }
}
