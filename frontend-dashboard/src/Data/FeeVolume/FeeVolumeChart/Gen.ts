import { Arbitrary, array, tuple } from "fast-check"

import * as FeeVolumeChart from "Data/FeeVolume/FeeVolumeChart"

import { genTimeInterval } from "Data/TimeInterval/Gen"
import { genFeeVolume } from "Data/FeeVolume/Gen"

export const genVolumeChartItem: Arbitrary<FeeVolumeChart.Item.Type> = tuple(
  genTimeInterval,
  genFeeVolume
)

export const genVolumeChart: Arbitrary<FeeVolumeChart.Type> =
  array(genVolumeChartItem)
