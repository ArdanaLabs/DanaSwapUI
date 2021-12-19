import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import { updateMyVaultsAction, updateWalletAddressAction } from "./actions"
import { VaultInfo, WalletState } from "./reducer"

export function useWallet(): any {
  const dispatch = useDispatch<AppDispatch>()

  const { address, balance, myVaults } = useSelector<AppState, WalletState>(
    (state) => state.wallet,
    shallowEqual
  )

  const updateWalletAddress = useCallback(
    (address: string) => {
      dispatch(updateWalletAddressAction({ address }))
    },
    [dispatch]
  )

  const updateMyVaults = useCallback(
    (vaults: VaultInfo[]) => {
      dispatch(updateMyVaultsAction({ vaults }))
    },
    [dispatch]
  )

  return {
    address,
    balance,
    myVaults,
    updateWalletAddress,
    updateMyVaults,
  }
}
