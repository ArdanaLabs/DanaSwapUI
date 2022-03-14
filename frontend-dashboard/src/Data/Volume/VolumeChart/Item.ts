import * as T from "io-ts/Type"

import * as ByTxType from "Data/ByTxType"
import * as TimeInterval from "Data/TimeInterval"
import * as Volume from "Data/Volume"

/*
 * @category model
 */
export type VolumeChartItem = [TimeInterval.Type, ByTxType.Type<Volume.Type>]

export type Type = VolumeChartItem

export const codec = T.tuple(TimeInterval.codec, ByTxType.codec(Volume.codec))
