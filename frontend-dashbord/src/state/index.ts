import { configureStore, getDefaultMiddleware } from "@reduxjs/toolkit";
import { save, load } from "redux-localstorage-simple";
import user from "./user/reducer";
import home from "./home/reducer";
import chart from "./chart/reducer";
import wallet from "./wallet/reducer";
import loader from "./loader/reducer";

const PERSISTED_KEYS: string[] = ["user"];

const store = configureStore({
  reducer: {
    user,
    home,
    chart,
    loader,
    wallet,
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
    disableWarnings: true,
  }),
});

export default store;

export type AppState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
