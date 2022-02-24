// import { useCallback } from 'react';
import { useDispatch, useSelector } from "react-redux"

import * as O from "fp-ts/Option"
import { pipe } from "fp-ts/function"

import * as Theme from "Data/User/Theme"

import { AppDispatch, AppState } from "state"
import { UserState } from "./reducer"
import { updateUserTheme } from "./actions"

export function getTheme(
  userState: Pick<UserState, "theme" | "prefersColorScheme">
): Theme.Theme {
  return pipe(
    userState.theme,
    O.alt(
      (): O.Option<Theme.Theme> =>
        O.chain(
          (s: Theme.SupportedW3ColorScheme): O.Option<Theme.Theme> =>
            Theme.fromW3ColorScheme(s)
        )(userState.prefersColorScheme)
    ),
    O.getOrElse((): Theme.Theme => Theme.defaultTheme)
  )
}

export function useUserTheme(): Theme.Theme {
  const user: UserState = useSelector<AppState, UserState>(
    (state) => state.user
  )
  return getTheme(user)
}

export function useUserThemeManager(): [
  Theme.Theme,
  (newTheme: Theme.Theme) => void
] {
  const dispatch = useDispatch<AppDispatch>()
  const theme: Theme.Theme = useUserTheme()

  // const setDarkMode = useCallback(
  //   (darkMode: boolean) => {
  //     dispatch(updateUserDarkMode({ userDarkMode: darkMode }));
  //   },
  //   [dispatch]
  // );
  const setTheme = (newTheme: Theme.Theme): void => {
    dispatch(updateUserTheme(newTheme))
  }

  return [theme, setTheme]
}
