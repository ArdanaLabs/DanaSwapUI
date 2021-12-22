import { useCallback } from "react"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import {
  updateBalanceAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
} from "./actions"
import { fetchBalanceApi, fetchMyVaultsApi, getWalletAddress } from "./apis"
import { WalletState } from "./reducer"

export function useWallet(): any {
  const dispatch = useDispatch<AppDispatch>()

  const { address, balance, myVaults } = useSelector<AppState, WalletState>(
    (state) => state.wallet,
    shallowEqual
  )

  const updateWalletAddress = useCallback(async () => {
    const address = await getWalletAddress()
    dispatch(updateWalletAddressAction(address))
  }, [dispatch])

  const updateBalance = useCallback(async () => {
    const balance = await fetchBalanceApi()
    dispatch(updateBalanceAction(balance))
  }, [dispatch])

  const updateMyVaults = useCallback(async () => {
    const vaults = await fetchMyVaultsApi()
    dispatch(updateMyVaultsAction(vaults))
  }, [dispatch])

  return {
    address,
    balance,
    myVaults,
    updateWalletAddress,
    updateBalance,
    updateMyVaults,
  }
}
