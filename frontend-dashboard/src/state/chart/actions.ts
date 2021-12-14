import { createAction } from "@reduxjs/toolkit"
import * as Option from "fp-ts/Option"

export interface RangedVolume {
  start: Option.Option<string>
  end: Option.Option<string>
  addLiquidity: Option.Option<number>
  removeLiquidity: Option.Option<number>
  total: Option.Option<number>
  trade: Option.Option<number>
}

export interface RangedLiquidity {
  start: Option.Option<string>
  end: Option.Option<string>
  value: Option.Option<number>
}

export interface RangedFees {
  start: Option.Option<string>
  end: Option.Option<string>
  value: Option.Option<number>
}

export interface RangedTxCount {
  start: Option.Option<string>
  end: Option.Option<string>
  addLiquidity: Option.Option<number>
  removeLiquidity: Option.Option<number>
  total: Option.Option<number>
  trade: Option.Option<number>
}

export interface RangedAPY {
  start: Option.Option<string>
  end: Option.Option<string>
  value: Option.Option<number>
}

export interface RangedTransactions {
  tx: {
    tag: Option.Option<string> // DepositTx, WithdrawalTx, TradeTx
    contents: {
      counterpartyAddress: Option.Option<string>
      created: Option.Option<string>
      spentAsset: Option.Option<string> // TradeTx
      purchasedAsset: Option.Option<string> // TradeTx
      spentAmount: Option.Option<number> // TradeTx
      purchasedAmount: Option.Option<number> // TradeTx
      amounts: {
        // DepositTx, WithdrawalTx
        [token: string]: Option.Option<number>
      }
    }
  }
  navUSD: Option.Option<number>
}

export const updateAggVolume = createAction<RangedVolume[]>(
  "home/updateAggVolume"
)

export const updateAggLiquidity = createAction<RangedLiquidity[]>(
  "home/updateAggLiquidity"
)

export const updatePoolFees = createAction<RangedFees[]>("home/updatePoolFees")

export const updatePoolVolume = createAction<RangedVolume[]>(
  "home/updatePoolVolume"
)

export const updatePoolLiquidity = createAction<RangedLiquidity[]>(
  "home/updatePoolLiquidity"
)

export const updatePoolTXCount = createAction<RangedTxCount[]>(
  "home/updatePoolTXCount"
)

export const updatePoolAPY = createAction<RangedAPY[]>("home/updatePoolAPY")

export const updatePoolTransactions = createAction<RangedTransactions[]>(
  "home/updatePoolTransactions"
)
