import {
  ActionReducerMapBuilder,
  PayloadAction,
  createReducer,
} from "@reduxjs/toolkit"

import { RemoteData, fromEither, pending } from "fp-ts-remote-data"

import { CombinedStats } from "Data/Stats/CombinedStats"
import { FetchDecodeError, FetchDecodeResult } from "Data/FetchDecode"

import { receivedCombinedStats } from "./actions"

export type State = RemoteData<FetchDecodeError, CombinedStats>

export const initialState = pending

export default createReducer(
  initialState,
  (builder: ActionReducerMapBuilder<State>) =>
    builder.addCase(
      receivedCombinedStats,
      (
        _state: State,
        action: PayloadAction<FetchDecodeResult<CombinedStats>>
      ) => fromEither(action.payload)
    )
)
