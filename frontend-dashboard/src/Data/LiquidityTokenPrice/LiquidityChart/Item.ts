import * as T from "io-ts/Type"

import * as TimeInterval from "Data/TimeInterval"
import * as LiquidityTokenPrice from "Data/LiquidityTokenPrice"

export type LiquidityChartItem = [TimeInterval.Type, LiquidityTokenPrice.Type]

export type Type = LiquidityChartItem

export const codec = T.tuple(TimeInterval.codec, LiquidityTokenPrice.codec)
