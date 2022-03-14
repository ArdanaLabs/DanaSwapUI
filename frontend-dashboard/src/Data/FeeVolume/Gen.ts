import { Arbitrary } from "fast-check"

import * as FeeVolume from "Data/FeeVolume"

import { genPercent } from "Data/Unit/Percent/Gen"

export const genFeeVolume: Arbitrary<FeeVolume.Type> = genPercent.map(
  FeeVolume.iso.wrap
)
