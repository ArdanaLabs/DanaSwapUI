import { createReducer } from "@reduxjs/toolkit";

import { getAccountKeys } from "./actions";

export interface WalletStateType {
  privateKey: string | null;
  publicKey: string | null;
  rewardAddress: string | null;
  accountId: string | null;
  accountIdFull: string | null;
}

export const initialState: WalletStateType = {
  privateKey: null,
  publicKey: null,
  rewardAddress: null,
  accountId: null,
  accountIdFull: null,
};

export default createReducer(initialState, (builder) =>
  builder.addCase(getAccountKeys, (state, action) => {
    state.privateKey = action.payload.privateKey;
    state.publicKey = action.payload.publicKey;
    state.rewardAddress = action.payload.rewardAddress;
    state.accountId = action.payload.accountId;
    state.accountIdFull = action.payload.accountIdFull;
  })
);
