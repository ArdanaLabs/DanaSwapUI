import { Arbitrary, oneof, constant } from "fast-check"
import { Theme, SupportedW3ColorScheme } from "."

export const genTheme: Arbitrary<Theme> = oneof(
  constant(Theme.Light),
  constant(Theme.Dark)
)

export const genSupportedW3ColorScheme: Arbitrary<SupportedW3ColorScheme> =
  oneof(
    constant(SupportedW3ColorScheme.light),
    constant(SupportedW3ColorScheme.dark)
  )
