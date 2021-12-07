import { createReducer } from "@reduxjs/toolkit";

import { TotalStat, updateTotalStats } from "./actions";

export const initialState: TotalStat = {
  totalDepositsAllPoolsUSD: 0,
  totalDailyVolumeUSD: null,
  totalDailyTxCount: 0,
  totalDailyFeeVolumeUSD: 0,
  totalLiquidityUtilization: 0,
  poolStats: null,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateTotalStats, (state, action) => {
    const {
      totalDepositsAllPoolsUSD,
      totalDailyVolumeUSD,
      totalDailyTxCount,
      totalDailyFeeVolumeUSD,
      totalLiquidityUtilization,
      poolStats,
    } = action.payload;

    state.totalDepositsAllPoolsUSD = totalDepositsAllPoolsUSD;
    state.totalDailyVolumeUSD = totalDailyVolumeUSD;
    state.totalDailyTxCount = totalDailyTxCount;
    state.totalDailyFeeVolumeUSD = totalDailyFeeVolumeUSD;
    state.totalLiquidityUtilization = totalLiquidityUtilization;

    state.poolStats = poolStats;
  })
);
