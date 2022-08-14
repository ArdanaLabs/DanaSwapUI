export interface VaultInfo {
  //  TBD
  id: number
  asset: string
  type: string
  locked: number
  stabilityFee: number
  liquidationFee: number
  minCollRatio: number

  dustLimit: number

  //
  isPopular: boolean
  isStableCoin: boolean
  isLP: boolean
}

export interface VaultState {
  vaults: VaultInfo[]
}
