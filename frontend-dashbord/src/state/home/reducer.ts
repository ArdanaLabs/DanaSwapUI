import { createReducer } from "@reduxjs/toolkit";

import { TotalStat, updateTotalStats } from "./actions";

export const initialState: TotalStat = {
  totalDepositsAllPoolsUSD: 0,
  totalDailyVolumeUSD: 0,
  totalDailyTxCount: 0,
  totalDailyFeeVolumeUSD: 0,
  poolStats: null,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateTotalStats, (state, action) => {
    const {
      totalDepositsAllPoolsUSD,
      totalDailyVolumeUSD,
      totalDailyTxCount,
      totalDailyFeeVolumeUSD,
      poolStats,
    } = action.payload;

    state.totalDepositsAllPoolsUSD = totalDepositsAllPoolsUSD;
    state.totalDailyVolumeUSD = totalDailyVolumeUSD;
    state.totalDailyTxCount = totalDailyTxCount;
    state.totalDailyFeeVolumeUSD = totalDailyFeeVolumeUSD;

    state.poolStats = poolStats;
  })
);
