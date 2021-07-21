import { createAction } from "@reduxjs/toolkit";

export interface AggVol {
  start: string | null;
  end: string | null;
  addLiquidity: number | null;
  removeLiquidity: number | null;
  total: number | null;
  trade: number | null;
}

export const updateAggVol = createAction<AggVol>(
  "home/updateAggVol"
);
