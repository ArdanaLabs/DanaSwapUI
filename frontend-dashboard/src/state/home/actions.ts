import { createAction } from "@reduxjs/toolkit";

export interface PoolStat {
  [poolName: string]: {
    recentDailyAPYPercent: number | null; // (unit: %)
    recentWeeklyAPYPercent: number | null; // (unit: %)
    recentMonthlyAPYPercent: number | null; // (unit: %)
    recentAnnualAPYPercent: number | null; // (unit: %)
    totalAPYPercent: number | null; // (unit: %)
    recentDailyVolumeUSD: {
      addLiquidity: number | null;
      removeLiquidity: number | null;
      total: number | null;
      trade: number | null;
    };

    reserves: {
      [currencyName: string]: number | null;
    };

    feePercent: number | null; // (unit: %)
    adminFeePercent: number | null; // (unit: %)
    virtualPriceUSD: number | null; // (unit: USD)
    liquidityUtilization: number | null; // (unit: USD (volume) / USD (liquidity))
    amplificationCoefficient: number | null;
    dailyTxCount: number | null;
    dailyFeeVolumeUSD: number | null;
    navUSD: number | null;
  };
}

export interface VolumeUSD {
  addLiquidity: number | null;
  removeLiquidity: number | null;
  total: number | null;
  trade: number | null;
}

export interface TotalStat {
  totalDailyTxCount: number | null;
  totalDailyFeeVolumeUSD: number| null;
  totalDailyVolumeUSD: VolumeUSD | null; // (unit: USD)
  totalDepositsAllPoolsUSD: number | null;
  totalLiquidityUtilization: number | null;
  poolStats: PoolStat | null;
}

export const updateTotalStats = createAction<TotalStat>(
  "home/updateTotalStats"
);
