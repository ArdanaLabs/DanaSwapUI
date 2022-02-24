import { testProp } from "jest-fast-check"

import * as O from "fp-ts/Option"
import { getOption } from "fp-ts-laws/lib/Option"

import * as Theme from "Data/User/Theme"
import { genTheme, genSupportedW3ColorScheme } from "Data/User/Theme/Gen"

import reducer, { initialState } from "./reducer"
import { updatePrefersColorScheme, updateUserTheme } from "./actions"

describe("User reducers", () => {
  testProp(
    "should return prefersColorScheme value when dispatch updatePrefersColorScheme action",
    [getOption(genSupportedW3ColorScheme)],
    (oscheme: O.Option<Theme.SupportedW3ColorScheme>) => {
      expect(
        reducer(initialState, updatePrefersColorScheme(oscheme))
          .prefersColorScheme
      ).toEqual(oscheme)
    }
  )

  testProp(
    "should return theme value when dispatch updateUserTheme action",
    [genTheme],
    (theme: Theme.Theme) => {
      expect(reducer(initialState, updateUserTheme(theme)).theme).toEqual(
        O.some(theme)
      )
    }
  )
})
