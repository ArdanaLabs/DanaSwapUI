import { Arbitrary, float } from "fast-check"

import * as APY from "Data/APY"

export const genAPY: Arbitrary<APY.Type> = float().map(APY.iso.wrap)
