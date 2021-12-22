import { VaultInfo } from "./reducer"

export const fetchVaultsApi = async (): Promise<VaultInfo[]> => {
  // mock api
  return [
    {
      id: 0,
      asset: "YIFI1",
      assetLogo: require("assets/image/coins/dusd.svg").default,
      type: "WBTC-2",
      locked: 13794.18,
      stabilityFee: 0.02,
      minCollRatio: 1.5,
    },
    {
      id: 1,
      asset: "YIFI2",
      assetLogo: require("assets/image/coins/dusd.svg").default,
      type: "WBTC-3",
      locked: 13794.18,
      stabilityFee: 0.03,
      minCollRatio: 2.5,
    },
  ]
}
