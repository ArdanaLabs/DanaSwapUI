import { createReducer } from "@reduxjs/toolkit";
import { 
  updateAggVolume,
  updateAggLiquidity,
  updatePoolFees,
  updatePoolVolume,
  updatePoolLiquidity,
  updatePoolTxCount,
} from "./actions";

export const initialState: any = {
  aggVolume: null,
  aggLiquidity: null,
  poolFees: null,
  poolVolume: null,
  poolLiquidity: null,
  poolTxCount: null,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateAggVolume, (state, action) => {
    state.aggVolume = action.payload;
  }).addCase(updateAggLiquidity, (state, action) => {
    state.aggLiquidity = action.payload;
  }).addCase(updatePoolFees, (state, action) => {
    state.poolFees = action.payload;
  }).addCase(updatePoolVolume, (state, action) => {
    state.poolVolume = action.payload;
  }).addCase(updatePoolLiquidity, (state, action) => {
    state.poolLiquidity = action.payload;
  }).addCase(updatePoolTxCount, (state, action) => {
    state.poolTxCount = action.payload;
  })
);
