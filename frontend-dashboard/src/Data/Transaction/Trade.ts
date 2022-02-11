import * as IO from "io-ts"
import * as Trade_ from "Data/Trade"

export const codec = IO.strict(
  {
    _tag: IO.literal("Trade"),
    trade: Trade_.codec,
  },
  "Trade"
)

export type Trade = IO.TypeOf<typeof codec>

export type Type = Trade
