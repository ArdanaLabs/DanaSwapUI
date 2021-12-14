import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import { updateWalletAddressAction } from "./actions"
import { WalletState } from "./reducer"

export function useWallet(): any {
  const dispatch = useDispatch<AppDispatch>()

  const { address, balance } = useSelector<AppState, WalletState>(
    (state) => state.wallet,
    shallowEqual
  )

  const updateWalletAddress = useCallback(
    (address: string) => {
      dispatch(updateWalletAddressAction({ address }))
    },
    [dispatch]
  )

  return {
    address,
    balance,
    updateWalletAddress,
  }
}
