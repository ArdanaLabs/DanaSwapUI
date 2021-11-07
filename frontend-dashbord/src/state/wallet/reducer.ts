import { createReducer } from "@reduxjs/toolkit";
import { currencyFormatter } from "utils/currency";

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
      state.balances = action.payload.map((balance: Currency) => {
        switch (balance.unit) {
          case "lovelace":
            return {
              unit: "ada",
              quantity: currencyFormatter(balance.quantity),
            };
          case "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7":
            return {
              unit: "dana",
              quantity: balance.quantity,
            };
          default:
            return balance;
        }
      });
    })
    .addCase(getAddressAction, (state, action) => {
      state.address = action.payload;
    })
);
