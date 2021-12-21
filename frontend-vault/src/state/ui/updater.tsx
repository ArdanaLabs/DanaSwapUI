import React from "react"
import { useUiModal } from "./hooks"
import VaultModal from "components/Modal/VaultModal"

export default function Updater(): JSX.Element {
  const { modal, toggleModal } = useUiModal()

  const handleClose = () => {
    toggleModal({
      open: false,
    })
  }

  return <VaultModal info={modal} handleClose={handleClose} />
}
