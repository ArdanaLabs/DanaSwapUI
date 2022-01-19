import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import {
  updateBalanceAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
} from "./actions"
import { fetchBalanceApi, fetchMyVaultsApi, getWalletAddress } from "./apis"
import { MyVaultInfo, WalletState } from "./types"

export function useWallet(): {
  address: string
  balance: number
  myVaults: MyVaultInfo[]
  updateWalletAddress: () => Promise<void>
  updateBalance: () => Promise<void>
  updateMyVaults: () => Promise<void>
} {
  const dispatch = useDispatch<AppDispatch>()

  const { address, balance, myVaults } = useSelector<AppState, WalletState>(
    (state) => state.wallet,
    shallowEqual
  )

  const updateWalletAddress = async (): Promise<void> => {
    const address = await getWalletAddress()
    dispatch(updateWalletAddressAction(address))
  }

  const updateBalance = async (): Promise<void> => {
    const balance = await fetchBalanceApi(address)
    dispatch(updateBalanceAction(balance))
  }

  const updateMyVaults = async (): Promise<void> => {
    const vaults = await fetchMyVaultsApi(address)
    dispatch(updateMyVaultsAction(vaults))
  }

  return {
    address,
    balance,
    myVaults,
    updateWalletAddress,
    updateBalance,
    updateMyVaults,
  }
}
