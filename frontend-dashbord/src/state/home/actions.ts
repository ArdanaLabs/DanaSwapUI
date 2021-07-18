import { createAction } from "@reduxjs/toolkit";

export interface PoolStat {
  [poolName: string]: {
    recentDailyAPYPercent: number | null; // (unit: %)
    recentWeeklyAPYPercent: number | null; // (unit: %)
    recentMonthlyAPYPercent: number | null; // (unit: %)
    recentAnnualAPYPercent: number | null; // (unit: %)
    totalAPYPercent: number | null; // (unit: %)
    recentDailyVolumeUSD: number | null; // (unit: USD)

    reserves: {
      [currencyName: string]: number | null;
    };

    feePercent: number | null; // (unit: %)
    adminFeePercent: number | null; // (unit: %)
    virtualPriceUSD: number | null; // (unit: USD)
    liquidityUtilizationRatio: number | null; // (unit: USD (volume) / USD (liquidity))
  };
}

export interface TotalStat {
  totalDepositsAllPoolsUSD: number | null; // (unit: USD)
  totalDailyVolumeUSD: number | null; // (unit: USD)
  poolStats: PoolStat | null;
}

export const updateTotalStats = createAction<TotalStat>(
  "home/updateTotalStats"
);
