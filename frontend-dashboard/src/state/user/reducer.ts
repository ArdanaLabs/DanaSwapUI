import { ActionReducerMapBuilder, createReducer } from "@reduxjs/toolkit"

import * as O from "fp-ts/Option"

import { SupportedW3ColorScheme, Theme } from "Data/User/Theme"

import { updatePrefersColorScheme, updateUserTheme } from "./actions"

function currentTimestamp(): number {
  return new Date().getTime()
}

export const THEME_LOCALSTORAGE_KEY = "user/theme"

export interface UserState {
  lastUpdateVersionTimestamp?: number
  theme: O.Option<Theme> // current theme
  prefersColorScheme: O.Option<SupportedW3ColorScheme>
  timestamp: number
}

export const initialState: UserState = {
  theme: O.none,
  prefersColorScheme: O.none,
  timestamp: currentTimestamp(), // side-effect
}

export default createReducer(
  initialState,
  (builder: ActionReducerMapBuilder<UserState>) =>
    builder
      .addCase(updateUserTheme, (state, action) => {
        localStorage.setItem(
          THEME_LOCALSTORAGE_KEY,
          action.payload.toString(10)
        )
        const timestamp = currentTimestamp()
        return { ...state, theme: O.some(action.payload), timestamp }
      })
      .addCase(updatePrefersColorScheme, (state, action) => {
        const timestamp = currentTimestamp()
        return { ...state, prefersColorScheme: action.payload, timestamp }
      })
)
