import { createReducer } from "@reduxjs/toolkit";

import { updateAggVol } from "./actions";

export const initialState: any = {
  aggVolsPerFiveMins: [],
};

export default createReducer(initialState, (builder) =>
  builder.addCase(updateAggVol, (state, action) => {
  })
);
