import * as T from "io-ts/Type"

import * as TimeInterval from "Data/TimeInterval"
import * as FeeVolume from "Data/FeeVolume"

export type FeeVolumeChartItem = [TimeInterval.Type, FeeVolume.Type]

export type Type = FeeVolumeChartItem

export const codec = T.tuple(TimeInterval.codec, FeeVolume.codec)
