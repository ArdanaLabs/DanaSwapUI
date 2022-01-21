import { MyVaultInfo } from "./types"

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
