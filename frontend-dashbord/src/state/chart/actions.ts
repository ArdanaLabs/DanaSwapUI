import { createAction } from "@reduxjs/toolkit";

export interface RangedVolume {
  start: string | null;
  end: string | null;
  addLiquidity: number | null;
  removeLiquidity: number | null;
  total: number | null;
  trade: number | null;
}
export interface RangedLiquidity {
  start: string | null;
  end: string | null;
  value: number | null;
}
export interface RangedFees {
  start: string | null;
  end: string | null;
  value: number | null;
}
export interface RangedTxCount {
  start: string | null;
  end: string | null;
  addLiquidity: number | null;
  removeLiquidity: number | null;
  total: number | null;
  trade: number | null;
}

export const updateAggVolume = createAction<RangedVolume[]>(
  "home/updateAggVolume"
);

export const updateAggLiquidity = createAction<RangedLiquidity[]>(
  "home/updateAggLiquidity"
);

export const updatePoolFees = createAction<RangedFees[]>(
  "home/updatePoolFees"
);

export const updatePoolVolume = createAction<RangedVolume[]>(
  "home/updatePoolVolume"
);

export const updatePoolLiquidity = createAction<RangedLiquidity[]>(
  "home/updatePoolLiquidity"
);

export const updatePoolTxCount = createAction<RangedTxCount[]>(
  "home/updatePoolTxCount"
);
