import { createAction } from "@reduxjs/toolkit"

import * as O from "fp-ts/Option"

import { SupportedW3ColorScheme, Theme } from "Data/User/Theme"

export const updateUserTheme = createAction<Theme>("user/updateUserTheme")

export const updatePrefersColorScheme = createAction<
  O.Option<SupportedW3ColorScheme>
>("user/updatePrefersColorScheme")
