import { Arbitrary } from "fast-check"
import * as Volume from "Data/Volume"
import { genUSD } from "Data/Unit/USD/Gen"

export const genVolume: Arbitrary<Volume.Type> = genUSD.map(Volume.iso.wrap)
