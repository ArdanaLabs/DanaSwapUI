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
  address: string
  balance: number
  myVaults: MyVaultInfo[]
}
