import BigNumber from "bignumber.js"
import { shallowEqual, useDispatch, useSelector } from "react-redux"

import { AppDispatch, AppState } from "state"
import {
  getCardanoApiAction,
  updateBalanceAction,
  updateMyVaultsAction,
  updateWalletAddressAction,
} from "./actions"
import { fetchMyVaultsApi, getWalletAddress } from "./apis"
import { CardanoApi, MyVaultInfo, WalletState, WalletType } from "./types"

const cardano = (window as any).cardano

export function useWallet(): {
  cardanoApi: CardanoApi
  address: string
  balance: BigNumber
  myVaults: MyVaultInfo[]
  connectWallet: () => void
  updateWalletAddress: () => Promise<void>
  updateBalance: (balance: BigNumber) => Promise<void>
  updateMyVaults: () => Promise<void>
} {
  const dispatch = useDispatch<AppDispatch>()

  const { cardanoApi, address, balance, myVaults } = useSelector<
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

  const updateWalletAddress = async (): Promise<void> => {
    const address = await getWalletAddress()
    dispatch(updateWalletAddressAction(address))
  }

  const updateBalance = async (balance: BigNumber): Promise<void> => {
    dispatch(updateBalanceAction(balance))
  }

  const updateMyVaults = async (): Promise<void> => {
    const vaults = await fetchMyVaultsApi(address)
    dispatch(updateMyVaultsAction(vaults))
  }

  return {
    cardanoApi,
    address,
    balance,
    myVaults,
    connectWallet,
    updateWalletAddress,
    updateBalance,
    updateMyVaults,
  }
}
