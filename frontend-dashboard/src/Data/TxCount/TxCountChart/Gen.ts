import { Arbitrary, array, tuple } from "fast-check"

import { genByTxType } from "Data/ByTxType/Gen"
import { genTimeInterval } from "Data/TimeInterval/Gen"
import { genTxCount } from "Data/TxCount/Gen"

import * as TxCountChart from "./."

export const genTxCountChartItem: Arbitrary<TxCountChart.Item.Type> = tuple(
  genTimeInterval,
  genByTxType(genTxCount)
)

export const genTxCountChart: Arbitrary<TxCountChart.Type> =
  array(genTxCountChartItem)
