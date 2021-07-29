import { createReducer } from "@reduxjs/toolkit";

import { setLoading } from "./actions";

export const initialState = {
  loading: false
};

export default createReducer(initialState, (builder) =>
  builder.addCase(setLoading, (state, action) => {
    state.loading = action.payload;
  })
);
