import { MyVaultInfo } from "./reducer"

export const getWalletAddress = async (): Promise<string> => {
  // from Nami wallet
  return "0x2ddA6C07ED3671F8d2f19B317e91e4DFD43f6621"
}

export const fetchBalanceApi = async (address: string): Promise<number> => {
  // from Nami wallet
  return 0.23
}

export const fetchMyVaultsApi = async (
  address: string
): Promise<MyVaultInfo[]> => {
  try {
    const url: URL = new URL(
      `/myvaults?address=${address}`,
      process.env.REACT_APP_RPC_API
    )

    const response = await fetch(url.toString())
    const myVaults: MyVaultInfo[] = await response.json()
    return myVaults
  } catch (e) {
    console.error(e)
    return []
  }
}
