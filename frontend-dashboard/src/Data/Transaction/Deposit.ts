import * as IO from "io-ts"

import * as Transfer from "Data/Transfer"

export const codec = IO.strict(
  {
    _tag: IO.literal("Deposit"),
    deposit: Transfer.codec,
  },
  "Deposit"
)

export type Deposit = IO.TypeOf<typeof codec>

export type Type = Deposit
