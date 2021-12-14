import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import { updateWalletAddressAction } from "./actions"

export function useWallet(): [string, (address: string) => void] {
  const dispatch = useDispatch<AppDispatch>()

  const { address } = useSelector<AppState, { address: string }>(
    (state) => state.wallet,
    shallowEqual
  )

  const updateWalletAddress = useCallback(
    (address: string) => {
      dispatch(updateWalletAddressAction({ address }))
    },
    [dispatch]
  )

  return [address, updateWalletAddress]
}
