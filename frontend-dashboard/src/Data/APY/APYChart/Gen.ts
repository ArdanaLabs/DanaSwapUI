import { Arbitrary, array, tuple } from "fast-check"

import * as APYChart from "Data/APY/APYChart"

import { genTimeInterval } from "Data/TimeInterval/Gen"
import { genAPY } from "Data/APY/Gen"

export const genAPYChartItem: Arbitrary<APYChart.Item.Type> = tuple(
  genTimeInterval,
  genAPY
)

export const genAPYChart: Arbitrary<APYChart.Type> = array(genAPYChartItem)
