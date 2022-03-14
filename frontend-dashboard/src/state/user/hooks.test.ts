import { testProp } from "jest-fast-check"
// import { useSelector } from "react-redux"
// import configureMockStore from "redux-mock-store"

import * as O from "fp-ts/Option"
import { pipe } from "fp-ts/function"
import { getOption } from "fp-ts-laws/lib/Option"

import * as Theme from "Data/User/Theme"
import { genTheme, genSupportedW3ColorScheme } from "Data/User/Theme/Gen"

import { getTheme } from "./hooks"
// import { UserState, initialState } from "./reducer"

require("@relmify/jest-fp-ts")

/*
const mockStore = configureMockStore([])

let store: any
const initState: { user: UserState } = {
  user: initialState,
}

const mockDispatch = jest.fn()
jest.mock("react-redux", () => ({
  ...jest.requireActual("react-redux"),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch,
}))

beforeEach(() => {
  store = mockStore(initState)
})
*/

describe("User hooks", () => {
  /*
  beforeEach(() => {
    ;(useSelector as jest.Mock).mockImplementation((callback) => {
      return callback(initialState)
    })
  })

  afterEach(() => {
    ;(useSelector as jest.Mock).mockClear()
  })
  */

  describe("getTheme", () => {
    it("should pick default with nothing set", () => {
      const theme: Theme.Theme = getTheme({
        theme: O.none,
        prefersColorScheme: O.none,
      })
      expect(theme).toEqual(Theme.defaultTheme)
    })

    testProp(
      "should prefer the user agent to our default",
      [genSupportedW3ColorScheme],
      (scheme: Theme.SupportedW3ColorScheme) => {
        const theme: Theme.Theme = getTheme({
          theme: O.none,
          prefersColorScheme: O.some(scheme),
        })
        return O.fold(
          () => false,
          (p: Theme.Theme) => Theme.Eq.equals(theme, p)
        )(Theme.fromW3ColorScheme(scheme))
      }
    )

    testProp(
      "should prefer the explicitly set theme",
      [genTheme, getOption(genSupportedW3ColorScheme)],
      (
        theme_: Theme.Theme,
        oscheme: O.Option<Theme.SupportedW3ColorScheme>
      ) => {
        const theme: Theme.Theme = getTheme({
          theme: O.some(theme_),
          prefersColorScheme: oscheme,
        })
        return Theme.Eq.equals(theme, theme_)
      }
    )

    testProp(
      "should choose the appropriate theme for optional values",
      [getOption(genTheme), getOption(genSupportedW3ColorScheme)],
      (
        otheme: O.Option<Theme.Theme>,
        oscheme: O.Option<Theme.SupportedW3ColorScheme>
      ) => {
        const theme: Theme.Theme = getTheme({
          theme: otheme,
          prefersColorScheme: oscheme,
        })
        return Theme.Eq.equals(
          theme,
          pipe(
            otheme, // prefer explicit
            O.alt(() => O.chain(Theme.fromW3ColorScheme)(oscheme)), // prefer user agent
            O.getOrElse(() => Theme.defaultTheme) // else use default
          )
        )
      }
    )
  })
})
