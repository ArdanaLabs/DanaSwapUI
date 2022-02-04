import { Arbitrary, float } from "fast-check"
import * as AmplificationCoefficient from "."

export const genUSD: Arbitrary<AmplificationCoefficient.Type> = float().map(
  AmplificationCoefficient.iso.wrap
)
