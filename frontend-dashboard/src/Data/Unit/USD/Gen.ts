import { Arbitrary, float } from "fast-check"
import * as USD from "Data/Unit/USD"

export const genUSD: Arbitrary<USD.Type> = float().map(USD.iso.wrap)
