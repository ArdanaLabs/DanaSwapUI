import * as IO from "io-ts"
import * as T from "io-ts/Type"

import * as TimeInterval from "Data/TimeInterval"
import * as APY from "Data/APY"

export const codec = T.tuple(TimeInterval.codec, APY.codec)

export type APYChartItem = IO.TypeOf<typeof codec>

export type Type = APYChartItem
