import { createAction } from "@reduxjs/toolkit";
import BigNumber from "bignumber.js";

export interface BalanceType {
  unit: string;
  quantity: BigNumber;
}

export const getBalancesAction = createAction<BalanceType[]>(
  "wallet/getBalancesAction"
);
