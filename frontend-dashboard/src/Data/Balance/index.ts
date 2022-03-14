import * as IO from "io-ts"

import * as ActualBalance from "./ActualBalance"
import * as NominalBalance from "./NominalBalance"

export * as Actual from "./ActualBalance"
export * as Nominal from "./NominalBalance"

export const codec = IO.strict(
  {
    nominal: NominalBalance.codec,
    actual: ActualBalance.codec,
  },
  "Balances"
)

export type Balances = IO.TypeOf<typeof codec>

export type Type = Balances
