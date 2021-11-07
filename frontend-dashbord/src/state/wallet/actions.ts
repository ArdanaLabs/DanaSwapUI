import { createAction } from "@reduxjs/toolkit";

export interface Currency {
  unit: string;
  quantity: number;
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
