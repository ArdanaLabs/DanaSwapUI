import {
  ActionReducerMapBuilder,
  PayloadAction,
  createReducer,
} from "@reduxjs/toolkit"

import * as E from "fp-ts/Either"
import { RemoteData, fromEither, pending } from "fp-ts-remote-data"

import {
  receivedAggVolume,
  receivedAggLiquidity,
  receivedPoolFees,
  receivedPoolVolume,
  receivedPoolLiquidity,
  receivedPoolTxCount,
  receivedPoolAPY,
  receivedPoolTransactions,
} from "./actions"

import {
  APYChart,
  FeeVolumeChart,
  LiquidityChart,
  TxCountChart,
  VolumeChart,
} from "Data/Chart"
import { FetchDecodeError, FetchDecodeResult } from "Data/FetchDecode"
import * as Transaction from "Data/Transaction"

export type State = {
  aggLiquidity: RemoteData<FetchDecodeError, LiquidityChart.Type>
  aggVolume: RemoteData<FetchDecodeError, VolumeChart.Type>
  poolFees: RemoteData<FetchDecodeError, FeeVolumeChart.Type>
  poolVolume: RemoteData<FetchDecodeError, VolumeChart.Type>
  poolLiquidity: RemoteData<FetchDecodeError, LiquidityChart.Type>
  poolTxCount: RemoteData<FetchDecodeError, TxCountChart.Type>
  poolAPY: RemoteData<FetchDecodeError, APYChart.Type>
  poolTransactions: RemoteData<FetchDecodeError, Transaction.WithTotalValue[]>
}

export const initialState: State = {
  aggLiquidity: pending,
  aggVolume: pending,
  poolFees: pending,
  poolVolume: pending,
  poolLiquidity: pending,
  poolTxCount: pending,
  poolAPY: pending,
  poolTransactions: pending,
}

// side-effectful: function that logs on Left, and returns the typical fromEither
// TODO: move this to chain with the decoder stuff
function fromEitherLogFail<A>({
  payload,
}: PayloadAction<FetchDecodeResult<A>>): RemoteData<FetchDecodeError, A> {
  if (E.isLeft(payload)) {
    console.error("Fetch error:", payload.left)
  }
  return fromEither(payload)
}

export default createReducer(
  initialState,
  (builder: ActionReducerMapBuilder<State>) =>
    builder
      .addCase(receivedAggLiquidity, (state, action) => ({
        ...state,
        aggLiquidity: fromEitherLogFail(action),
      }))
      .addCase(receivedAggVolume, (state, action) => ({
        ...state,
        aggVolume: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolFees, (state, action) => ({
        ...state,
        poolFees: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolVolume, (state, action) => ({
        ...state,
        poolVolume: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolLiquidity, (state, action) => ({
        ...state,
        poolLiquidity: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolTxCount, (state, action) => ({
        ...state,
        poolTxCount: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolAPY, (state, action) => ({
        ...state,
        poolAPY: fromEitherLogFail(action),
      }))
      .addCase(receivedPoolTransactions, (state, action) => ({
        ...state,
        poolTransactions: fromEitherLogFail(action),
      }))
)
