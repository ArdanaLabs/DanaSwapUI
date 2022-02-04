import * as O from "fp-ts/Option"

import * as PoolSetName from "Data/Pool/PoolSetName"

import { configureStore } from "@reduxjs/toolkit"
import { save, load } from "redux-localstorage-simple"
import user from "./user/reducer"
import home from "./home/reducer"
import chart from "./chart/reducer"

const PERSISTED_KEYS: string[] = ["user"]

export const logger = (store: any) => (next: any) => (action: any) => {
  console.group(action.type)
  console.info("dispatching", action)
  let result = next(action)
  console.log("next state", store.getState())
  console.groupEnd()
  return result
}

const store = configureStore({
  reducer: {
    user,
    home,
    chart,
  },
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({
      thunk: false,
      immutableCheck: true,
      serializableCheck: false,
    }).concat(/*logger,*/ save({ states: PERSISTED_KEYS })),
  preloadedState: load({
    states: PERSISTED_KEYS,
    disableWarnings: true,
  }),
})

export default store

export type AppState = ReturnType<typeof store.getState>
export type AppDispatch = typeof store.dispatch
