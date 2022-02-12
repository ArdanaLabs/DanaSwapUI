import * as IO from "io-ts"

import * as Transfer from "Data/Transfer"

export const codec = IO.strict(
  {
    _tag: IO.literal("Withdrawal"),
    withdrawal: Transfer.codec,
  },
  "Withdrawal"
)

export type Withdrawal = IO.TypeOf<typeof codec>

export type Type = Withdrawal
