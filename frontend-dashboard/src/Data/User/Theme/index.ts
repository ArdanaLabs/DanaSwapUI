import * as O from "fp-ts/Option"
import * as Bounded_ from "fp-ts/Bounded"
import * as number from "fp-ts/number"
import * as Show_ from "fp-ts/Show"

export enum Theme {
  Light = 0,
  Dark,
  // Black,
}

export type Type = Theme

export const defaultTheme: Theme = Theme.Dark

export const allThemes: Theme[] = [Theme.Light, Theme.Dark]

export const Eq = number.Eq

export const Ord = number.Ord

export const Bounded: Bounded_.Bounded<Theme> = {
  equals: Eq.equals,
  compare: Ord.compare,
  top: Theme.Light,
  bottom: Theme.Dark,
}

export const Show: Show_.Show<Theme> = {
  show: (t: Theme): string => {
    switch (t) {
      case Theme.Light:
        return "Light"
      case Theme.Dark:
        return "Dark"
    }
  },
}

// https://pursuit.purescript.org/packages/purescript-enums/5.0.0/docs/Data.Enum

export function succ(theme: Theme): O.Option<Theme> {
  switch (theme) {
    case Theme.Light:
      return O.some(Theme.Dark)
    case Theme.Dark:
      return O.none
  }
}

export function pred(theme: Theme): O.Option<Theme> {
  switch (theme) {
    case Theme.Light:
      return O.none
    case Theme.Dark:
      return O.some(Theme.Light)
  }
}

// ---------------------------------------------------------------------------

// W3C expressed that they likely want to add to this list in the future
// A `string` would be preferred to having a type as it would be more flexible,
// but this isn’t how the listeners work right now. There is no way to ‘just get’
// the preferred scheme value.
export enum SupportedW3ColorScheme {
  light = "light",
  dark = "dark",
}

export const allSupportedW3ColorSchemes: SupportedW3ColorScheme[] = [
  SupportedW3ColorScheme.light,
  SupportedW3ColorScheme.dark,
]

export function fromW3ColorScheme(scheme: string): O.Option<Theme> {
  switch (scheme) {
    case "light":
      return O.some(Theme.Light)
    case "dark":
      return O.some(Theme.Dark)
    default:
      return O.none
  }
}
