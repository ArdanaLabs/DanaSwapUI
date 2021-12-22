import { MyVaultInfo } from "./reducer"

export const getWalletAddress = async (): Promise<string> => {
  // mock api
  return "0x2ddA6C07ED3671F8d2f19B317e91e4DFD43f6621"
}

export const fetchBalanceApi = async (): Promise<number> => {
  // mock api
  return 0.23
}

export const fetchMyVaultsApi = async (): Promise<MyVaultInfo[]> => {
  // mock api
  return [
    {
      id: 21,
      asset: "YIFI1",
      assetLogo: require("assets/image/coins/dusd.svg").default,
      type: "ER-2",
      locked: 13794.18,
      collRatio: 2.3,
      debt: 5602.59,
      risk: false,
    },
    {
      id: 32,
      asset: "YIFI2",
      assetLogo: require("assets/image/coins/dusd.svg").default,
      type: "ER-4",
      locked: 1794.18,
      collRatio: 1.3,
      debt: 562.59,
      risk: false,
    },
  ]
}
