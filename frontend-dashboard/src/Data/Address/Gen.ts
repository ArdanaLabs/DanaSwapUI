import { Arbitrary, string } from "fast-check"

import * as Address from "."

export const genAddress: Arbitrary<Address.Type> = string().map(
  Address.iso.wrap
)
