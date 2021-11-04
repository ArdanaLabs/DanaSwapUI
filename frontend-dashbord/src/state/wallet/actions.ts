import { createAction } from "@reduxjs/toolkit";
import BigNumber from "bignumber.js";

export interface BalanceType {
  unit: string;
  quantity: BigNumber;
}

export const getCardanoApiAction = createAction<any>(
  "wallet/getCardanoApiAction"
);

export const getAddressAction = createAction<string>(
  "wallet/getAddressAction"
);

export const getBalancesAction = createAction<BalanceType[]>(
  "wallet/getBalancesAction"
);
