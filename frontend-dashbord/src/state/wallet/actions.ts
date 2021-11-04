import { createAction } from "@reduxjs/toolkit";
import BigNumber from "bignumber.js";

export interface Currency {
  unit: string;
  quantity: BigNumber;
}

export const getCardanoApiAction = createAction<any>(
  "wallet/getCardanoApiAction"
);

export const getAddressAction = createAction<string>(
  "wallet/getAddressAction"
);

export const getBalancesAction = createAction<Currency[]>(
  "wallet/getBalancesAction"
);
