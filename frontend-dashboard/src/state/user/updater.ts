import { useEffect } from "react"
import { useDispatch } from "react-redux"

import * as O from "fp-ts/Option"
import { pipe } from "fp-ts/function"

import { Theme, SupportedW3ColorScheme } from "Data/User/Theme"

import { AppDispatch } from "state"
import { updatePrefersColorScheme, updateUserTheme } from "./actions"
import { THEME_LOCALSTORAGE_KEY } from "./reducer"

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>()

  useEffect(() => {
    const storedTheme: O.Option<Theme> = pipe(
      localStorage.getItem(THEME_LOCALSTORAGE_KEY),
      O.fromNullable,
      O.chain((s: string): O.Option<Theme> => {
        const i: number = parseInt(s, 10)
        if (Number.isNaN(i) || !(i in Theme)) {
          return O.none
        } else {
          return O.some(i)
        }
      })
    )

    if (O.isSome(storedTheme)) {
      dispatch(updateUserTheme(storedTheme.value))
    }

    // This matching is done in this manner because the W3 spec clearly states
    // that we should expect the current set scheme support to be extended.
    // However, there’s not an easy way to just ‘get’ the string value, unlike
    // `devicePixelRatio`, so getting this match isn’t ‘easy’.

    const mkMatcher = (scheme: SupportedW3ColorScheme): MediaQueryList =>
      window.matchMedia(`screen and (prefers-color-scheme: ${scheme})`)

    const lightMatch: MediaQueryList = mkMatcher(SupportedW3ColorScheme.light)
    const darkMatch: MediaQueryList = mkMatcher(SupportedW3ColorScheme.dark)

    function matchHandler(_evt?: MediaQueryListEvent) {
      let scheme: O.Option<SupportedW3ColorScheme> = O.none
      if (lightMatch.matches) {
        scheme = O.some(SupportedW3ColorScheme.light)
      } else if (darkMatch.matches) {
        scheme = O.some(SupportedW3ColorScheme.dark)
      }
      dispatch(updatePrefersColorScheme(scheme))
    }

    matchHandler()

    lightMatch.addEventListener("change", matchHandler)
    darkMatch.addEventListener("change", matchHandler)

    return () => {
      lightMatch.removeEventListener("change", matchHandler)
      darkMatch.removeEventListener("change", matchHandler)
    }
  }, [dispatch])

  return null
}
