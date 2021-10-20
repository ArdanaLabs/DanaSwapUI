import { createAction } from "@reduxjs/toolkit";

export interface AccountKeysType {
  privateKey: string | null;
  publicKey: string | null;
  rewardAddress: string | null;
  accountId: string | null;
  accountIdFull: string | null;
}

export const getAccountKeys = createAction<AccountKeysType>(
  "wallet/getAccountKeys"
);
