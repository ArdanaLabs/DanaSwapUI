import BigNumber from "bignumber.js"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import {
  getCardanoApiAction,
  updateBalancesAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
} from "./actions"
import { fetchMyVaultsApi } from "./apis"
import { CardanoApi, MyVaultInfo, WalletState, WalletType } from "./types"

const cardano = (window as any).cardano

export function useWallet() {
  const dispatch = useDispatch<AppDispatch>()

  const { cardanoApi, address, balances, myVaults } = useSelector<
    AppState,
    WalletState
  >((state) => state.wallet, shallowEqual)

  const checkWalletExtension = (
    walletType: string = WalletType.YOROI
  ): boolean => {
    if (!cardano) {
      return false
    }
    switch (walletType) {
      case WalletType.YOROI:
        return cardano.yoroi
      default:
        return false
    }
  }

  const connectWallet = (walletType: string = WalletType.YOROI): void => {
    if (!checkWalletExtension(walletType)) {
      alert("install Yoroi Wallet!")
      return
    }
    cardano.yoroi.enable().then((api: CardanoApi) => {
      dispatch(getCardanoApiAction(api))
    })
  }

  const updateWalletAddress = (newAddress: string): void => {
    dispatch(updateWalletAddressAction(newAddress))
  }

  const updateBalances = (balances: { [key: string]: BigNumber }): void => {
    dispatch(updateBalancesAction(balances))
  }

  const updateMyVaults = (): void => {
    fetchMyVaultsApi(address).then((vaults: MyVaultInfo[]) => {
      dispatch(updateMyVaultsAction(vaults))
    })
  }

  return {
    cardanoApi,
    address,
    balances,
    myVaults,
    connectWallet,
    updateWalletAddress,
    updateBalances,
    updateMyVaults,
  }
}
