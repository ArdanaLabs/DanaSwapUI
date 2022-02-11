import * as IO from "io-ts"
import { getLenses, optionFromNullable } from "io-ts-types"

import * as ByTxType from "Data/ByTxType"
import {
  TotalDeposits,
  TotalDailyVolume,
  TotalDailyFeeVolume,
  TotalDailyTxCount,
  TotalLiquidityUtilization,
} from "Data/Stats/AggregateStats"

import * as PoolStatsMap from "./PoolStatsMap"

export * as PoolStatsMap from "./PoolStatsMap"

export const codec = IO.type(
  {
    totalDepositsAllPoolsUSD: optionFromNullable(TotalDeposits.codec),
    totalDailyVolumeUSD: ByTxType.codec(
      optionFromNullable(TotalDailyVolume.codec)
    ),
    totalDailyFeeVolumeUSD: optionFromNullable(TotalDailyFeeVolume.codec),
    totalDailyTxCount: optionFromNullable(TotalDailyTxCount.codec),
    totalLiquidityUtilization: optionFromNullable(
      TotalLiquidityUtilization.codec
    ),
    poolStats: PoolStatsMap.codec,
  },
  "CombinedStats"
)

export type CombinedStats = IO.TypeOf<typeof codec>

export type Type = CombinedStats

export type TotalStats = Pick<
  CombinedStats,
  | "totalDepositsAllPoolsUSD"
  | "totalDailyVolumeUSD"
  | "totalDailyFeeVolumeUSD"
  | "totalDailyTxCount"
  | "totalLiquidityUtilization"
>

export function totalStatsFromCombined(combined: CombinedStats): TotalStats {
  return {
    totalDepositsAllPoolsUSD: combined.totalDepositsAllPoolsUSD,
    totalDailyVolumeUSD: combined.totalDailyVolumeUSD,
    totalDailyFeeVolumeUSD: combined.totalDailyFeeVolumeUSD,
    totalDailyTxCount: combined.totalDailyTxCount,
    totalLiquidityUtilization: combined.totalLiquidityUtilization,
  }
}

export const lenses = getLenses(codec)
