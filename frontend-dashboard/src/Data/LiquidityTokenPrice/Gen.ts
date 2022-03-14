import { Arbitrary } from "fast-check"

import * as LiquidityTokenPrice from "Data/LiquidityTokenPrice"

import { genUSD } from "Data/Unit/USD/Gen"

export const genLiquidityTokenPrice: Arbitrary<LiquidityTokenPrice.Type> =
  genUSD.map(LiquidityTokenPrice.iso.wrap)
