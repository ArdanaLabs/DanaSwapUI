import { Arbitrary, array, tuple } from "fast-check"

import * as LiquidityChart from "Data/LiquidityTokenPrice/LiquidityChart"

import { genTimeInterval } from "Data/TimeInterval/Gen"
import { genLiquidityTokenPrice } from "Data/LiquidityTokenPrice/Gen"

export const genLiquidityChartItem: Arbitrary<LiquidityChart.Item.Type> = tuple(
  genTimeInterval,
  genLiquidityTokenPrice
)

export const genLiquidityChart: Arbitrary<LiquidityChart.Type> = array(
  genLiquidityChartItem
)
