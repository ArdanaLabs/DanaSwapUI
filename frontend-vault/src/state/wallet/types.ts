import BigNumber from "bignumber.js"

export enum WalletType {
  YOROI = "yoroi",
}
export type CardanoApi = any

export interface MyVaultInfo {
  id: number
  asset: string
  type: string
  locked: number
  collRatio: number
  debt: number
  risk: boolean

  //
  isStableCoin: boolean
  isLP: boolean
}

export interface WalletState {
  cardanoApi: CardanoApi | null
  walletType: WalletType | null
  address: string
  balances: {
    [key: string]: BigNumber
  }
  myVaults: MyVaultInfo[]
}
