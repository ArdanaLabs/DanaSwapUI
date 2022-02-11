import { useDispatch, useSelector } from "react-redux"

import { fetchJSON } from "fp-fetch"
import * as E from "fp-ts/Either"
import * as IO from "fp-ts/IO"
import * as Task from "fp-ts/Task"
import * as TaskEither from "fp-ts/TaskEither"
import * as T from "io-ts/Type"

import { FetchDecodeTask, FetchDecodeResult } from "Data/FetchDecode"
import * as TimeInterval from "Data/TimeInterval"
import * as Transaction from "Data/Transaction"
import {
  APYChart,
  FeeVolumeChart,
  LiquidityChart,
  TxCountChart,
  VolumeChart,
} from "Data/Chart"
import { Granularity } from "Data/Chart/Granularity"
import { TransactionType } from "Data/Chart/TransactionType"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { apiURL, wrapWithStraightQuotationMarks } from "hooks"
import { AppDispatch, AppState } from "state"
import {
  receivedAggLiquidity,
  receivedAggVolume,
  receivedPoolAPY,
  receivedPoolFees,
  receivedPoolLiquidity,
  receivedPoolTransactions,
  receivedPoolTxCount,
  receivedPoolVolume,
} from "./actions"

export function useAggVolume() {
  const dispatch = useDispatch<AppDispatch>()
  const { aggVolume } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    aggVolume,
    fetchAggVolume: (
      range: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_aggVolume: FetchDecodeResult<VolumeChart.Type>) => {
          // side-effect
          E.isLeft(_aggVolume) &&
            console.error("Fetch processing error", _aggVolume.left)
          dispatch(receivedAggVolume(_aggVolume))
          return IO.of(_aggVolume)
        }
      )(fetchAggVolume(range, granularity))
    },
  }
}

export function useAggLiquidity() {
  const dispatch = useDispatch<AppDispatch>()
  const { aggLiquidity } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    aggLiquidity,
    fetchAggLiquidity: (
      range: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_aggLiquidity: FetchDecodeResult<LiquidityChart.Type>) => {
          // side-effect
          E.isLeft(_aggLiquidity) &&
            console.error("Fetch processing error", _aggLiquidity.left)
          dispatch(receivedAggLiquidity(_aggLiquidity))
          return IO.of(_aggLiquidity)
        }
      )(fetchAggLiquidity(range, granularity))
    },
  }
}

export function usePoolFees() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolFees } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    poolFees,
    fetchPoolFees: (
      poolSet: PoolSetName.Type,
      dateRange: TimeInterval.TimeInterval,
      granularity: Granularity
    ) =>
      Task.chainFirstIOK(
        (_poolFees: FetchDecodeResult<FeeVolumeChart.Type>) => {
          // side-effect
          E.isLeft(_poolFees) &&
            console.error("Fetch processing error", _poolFees.left)
          dispatch(receivedPoolFees(_poolFees))
          return IO.of(_poolFees)
        }
      )(fetchPoolFees(poolSet, dateRange, granularity)),
  }
}

export function usePoolVolume() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolVolume } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )
  return {
    poolVolume,
    fetchPoolVolume: (
      poolSet: PoolSetName.Type,
      dateRange: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_poolVolume: FetchDecodeResult<VolumeChart.Type>) => {
          // side-effect
          E.isLeft(_poolVolume) &&
            console.error("Fetch processing error", _poolVolume.left)
          dispatch(receivedPoolVolume(_poolVolume))
          return IO.of(_poolVolume)
        }
      )(fetchPoolVolume(poolSet, dateRange, granularity))
    },
  }
}

export function usePoolLiquidity() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolLiquidity } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )
  return {
    poolLiquidity,
    fetchPoolLiquidity: (
      poolSet: PoolSetName.Type,
      dateRange: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_poolLiquidity: FetchDecodeResult<LiquidityChart.Type>) => {
          // side-effect
          E.isLeft(_poolLiquidity) &&
            console.error("Fetch processing error", _poolLiquidity.left)
          dispatch(receivedPoolLiquidity(_poolLiquidity))
          return IO.of(_poolLiquidity)
        }
      )(fetchPoolLiquidity(poolSet, dateRange, granularity))
    },
  }
}

export function usePoolTxCount() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolTxCount } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    poolTxCount,
    fetchPoolTxCount: (
      poolSet: PoolSetName.Type,
      dateRange: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_poolTxCount: FetchDecodeResult<TxCountChart.Type>) => {
          // side-effect
          E.isLeft(_poolTxCount) &&
            console.error("Fetch processing error", _poolTxCount.left)
          dispatch(receivedPoolTxCount(_poolTxCount))
          return IO.of(_poolTxCount)
        }
      )(fetchPoolTxCount(poolSet, dateRange, granularity))
    },
  }
}

export function usePoolAPY() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolAPY } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    poolAPY,
    fetchPoolAPY: (
      poolSet: PoolSetName.Type,
      dateRange: TimeInterval.TimeInterval,
      granularity: Granularity
    ) => {
      return Task.chainFirstIOK(
        (_poolAPY: FetchDecodeResult<APYChart.Type>) => {
          // side-effect
          E.isLeft(_poolAPY) &&
            console.error("Fetch processing error", _poolAPY.left)
          dispatch(receivedPoolAPY(_poolAPY))
          return IO.of(_poolAPY)
        }
      )(fetchPoolAPY(poolSet, dateRange, granularity))
    },
  }
}

export function usePoolTransactions() {
  const dispatch = useDispatch<AppDispatch>()
  const { poolTransactions } = useSelector<AppState, AppState["chart"]>(
    (state) => state.chart
  )

  return {
    poolTransactions,
    fetchPoolTransactions: (
      pool: PoolSetName.Type,
      start: number,
      end: number,
      type: TransactionType
    ) => {
      return Task.chainFirstIOK(
        (
          _poolTransactions: FetchDecodeResult<Transaction.WithTotalValue[]>
        ) => {
          // side-effect
          E.isLeft(_poolTransactions) &&
            console.error("Fetch processing error", _poolTransactions.left)
          dispatch(receivedPoolTransactions(_poolTransactions))
          return IO.of(_poolTransactions)
        }
      )(fetchPoolTransactions(pool, start | 0, end | 0, type))
    },
  }
}

// fetch from api server
export function fetchAggVolume(
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<VolumeChart.Type> {
  const url: URL = apiURL("/chart/aggregate/volume", {
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(VolumeChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchAggLiquidity(
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<LiquidityChart.Type> {
  const url: URL = apiURL("/chart/aggregate/liquidity", {
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(LiquidityChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolFees(
  poolSet: PoolSetName.Type,
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<FeeVolumeChart.Type> {
  const url: URL = apiURL("/chart/pool/fees", {
    poolSet: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(FeeVolumeChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolVolume(
  poolSet: PoolSetName.Type,
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<VolumeChart.Type> {
  const url: URL = apiURL("/chart/pool/volume", {
    poolSet: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(VolumeChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolLiquidity(
  poolSet: PoolSetName.Type,
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<LiquidityChart.Type> {
  const url: URL = apiURL("/chart/pool/liquidity", {
    poolSet: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(LiquidityChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolTxCount(
  poolSet: PoolSetName.Type,
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<TxCountChart.Type> {
  const url: URL = apiURL("/chart/pool/tx-count", {
    poolSet: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(TxCountChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolAPY(
  poolSet: PoolSetName.Type,
  dateRange: TimeInterval.TimeInterval,
  granularity: Granularity
): FetchDecodeTask<APYChart.Type> {
  const url: URL = apiURL("/chart/pool/apy", {
    poolSet: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: TimeInterval._start.get(dateRange).toISOString(),
    end: TimeInterval._end.get(dateRange).toISOString(),
    grain: wrapWithStraightQuotationMarks(granularity),
  })

  return TaskEither.chainEitherKW(APYChart.codec.decode)(
    fetchJSON(url.toString())
  )
}

export function fetchPoolTransactions(
  poolSet: PoolSetName.Type,
  start: number,
  end: number,
  type: TransactionType
): FetchDecodeTask<Transaction.WithTotalValue[]> {
  const url: URL = apiURL("/transactions", {
    pool: wrapWithStraightQuotationMarks(PoolSetName.iso.unwrap(poolSet)),
    start: `${start | 0}`,
    end: `${end | 0}`,
    type: wrapWithStraightQuotationMarks(type),
  })

  return TaskEither.chainEitherKW(
    T.array(Transaction.withTotalValueAdaptedFromAeson).decode
  )(fetchJSON(url.toString()))
}
