import { createReducer } from "@reduxjs/toolkit";

import { BalanceType, getBalancesAction } from "./actions";

export interface WalletStateType {
  balances: BalanceType[]
}

export const initialState: WalletStateType = {
  balances: []
};

export default createReducer(initialState, (builder) =>
  builder.addCase(getBalancesAction, (state, action) => {
    state.balances = action.payload;
  })
);
