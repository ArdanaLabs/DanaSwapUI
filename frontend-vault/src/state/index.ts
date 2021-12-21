import { configureStore, getDefaultMiddleware } from "@reduxjs/toolkit"
import { save, load } from "redux-localstorage-simple"
import user from "./user/reducer"
import wallet from "./wallet/reducer"
import ui from "./ui/reducer"

const PERSISTED_KEYS: string[] = ["user"]

const store = configureStore({
  reducer: {
    user,
    wallet,
    ui,
  },
  middleware: [
    ...getDefaultMiddleware({
      thunk: false,
      immutableCheck: false,
      serializableCheck: false,
    }),
    save({ states: PERSISTED_KEYS }),
  ],
  preloadedState: load({
    states: PERSISTED_KEYS,
    // disableWarnings: true,
  }),
})

export default store

export type AppState = ReturnType<typeof store.getState>
export type AppDispatch = typeof store.dispatch
