import { createReducer } from "@reduxjs/toolkit";

import { TotalState, updateTotalStates } from "./actions";

export const initialState: TotalState = {
  totalDepositsAllPoolsUSD: 0,
  totalDailyVolumeUSD: 0,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateTotalStates, (state, action) => {
    const { totalDepositsAllPoolsUSD, totalDailyVolumeUSD } = action.payload;

    state.totalDepositsAllPoolsUSD = totalDepositsAllPoolsUSD;
    state.totalDailyVolumeUSD = totalDailyVolumeUSD;
  })
);
