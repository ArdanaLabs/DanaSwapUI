import { createReducer } from "@reduxjs/toolkit";

import {
  Currency,
  getCardanoApiAction,
  getAddressAction,
  getBalancesAction,
} from "./actions";

export interface WalletStateType {
  address: string;
  balances: Currency[];
  cardanoApi: any;
}

export const initialState: WalletStateType = {
  address: "",
  balances: [],
  cardanoApi: null,
};

export default createReducer(initialState, (builder) =>
  builder
    .addCase(getCardanoApiAction, (state, action) => {
      state.cardanoApi = action.payload;
    })
    .addCase(getBalancesAction, (state, action) => {
      state.balances = action.payload;
    })
    .addCase(getAddressAction, (state, action) => {
      state.address = action.payload;
    })
);