import { Arbitrary, float } from "fast-check"
import * as Percent from "Data/Unit/Percent"

export const genPercent: Arbitrary<Percent.Type> = float().map(Percent.iso.wrap)
