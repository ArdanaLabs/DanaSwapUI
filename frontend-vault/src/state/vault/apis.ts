import { VaultInfo } from "./reducer"

export const fetchVaultsApi = async (): Promise<VaultInfo[]> => {
  try {
    const url: URL = new URL("/vaults", process.env.REACT_APP_RPC_API)

    const response = await fetch(url.toString())
    const vaults: VaultInfo[] = await response.json()
    return vaults
  } catch (e) {
    console.error(e)
    return []
  }
}
