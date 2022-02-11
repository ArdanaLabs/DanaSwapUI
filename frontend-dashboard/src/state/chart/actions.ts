import { createAction } from "@reduxjs/toolkit"

import {
  APYChart,
  FeeVolumeChart,
  LiquidityChart,
  TxCountChart,
  VolumeChart,
} from "Data/Chart"
import { FetchDecodeResult } from "Data/FetchDecode"
import * as Transaction from "Data/Transaction"

export const receivedAggVolume = createAction<
  FetchDecodeResult<VolumeChart.Type>
>("home/receivedAggVolume")

export const receivedAggLiquidity = createAction<
  FetchDecodeResult<LiquidityChart.Type>
>("home/receivedAggLiquidity")

export const receivedPoolFees = createAction<
  FetchDecodeResult<FeeVolumeChart.Type>
>("home/receivedPoolFees")

export const receivedPoolVolume = createAction<
  FetchDecodeResult<VolumeChart.Type>
>("home/receivedPoolVolume")

export const receivedPoolLiquidity = createAction<
  FetchDecodeResult<LiquidityChart.Type>
>("home/receivedPoolLiquidity")

export const receivedPoolTxCount = createAction<
  FetchDecodeResult<TxCountChart.Type>
>("home/receivedPoolTxCount")

export const receivedPoolAPY = createAction<FetchDecodeResult<APYChart.Type>>(
  "home/receivedPoolAPY"
)

export const receivedPoolTransactions = createAction<
  FetchDecodeResult<Transaction.WithTotalValue[]>
>("home/receivedPoolTransactions")
