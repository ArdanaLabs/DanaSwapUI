import { createAction } from "@reduxjs/toolkit";

export interface TotalState {
  totalDepositsAllPoolsUSD: number | null;
  totalDailyVolumeUSD: number | null;
}

export const updateTotalStates = createAction<TotalState>(
  "home/updateTotalStates"
);
