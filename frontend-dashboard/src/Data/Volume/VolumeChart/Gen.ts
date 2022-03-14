import { Arbitrary, array, tuple } from "fast-check"

import { genVolume } from "Data/Volume/Gen"
import { genTimeInterval } from "Data/TimeInterval/Gen"
import { genByTxType } from "Data/ByTxType/Gen"

import * as VolumeChart from "Data/Volume/VolumeChart"

export const genVolumeChartItem: Arbitrary<VolumeChart.Item.Type> = tuple(
  genTimeInterval,
  genByTxType(genVolume)
)

export const genVolumeChart: Arbitrary<VolumeChart.Type> =
  array(genVolumeChartItem)
