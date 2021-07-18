import { createReducer } from "@reduxjs/toolkit";

import { TotalStat, updateTotalStats } from "./actions";

export const initialState: TotalStat = {
  totalDepositsAllPoolsUSD: 0,
  totalDailyVolumeUSD: 0,
  poolStats: null,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateTotalStats, (state, action) => {
    const { totalDepositsAllPoolsUSD, totalDailyVolumeUSD, poolStats } =
      action.payload;

    state.totalDepositsAllPoolsUSD = totalDepositsAllPoolsUSD;
    state.totalDailyVolumeUSD = totalDailyVolumeUSD;

    state.poolStats = poolStats;
  })
);
