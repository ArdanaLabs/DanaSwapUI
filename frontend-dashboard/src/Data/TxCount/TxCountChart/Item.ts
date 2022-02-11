import * as T from "io-ts/Type"

import * as ByTxType from "Data/ByTxType"
import * as TimeInterval from "Data/TimeInterval"
import * as TxCount from "Data/TxCount"

export type TxCountChartItem = [TimeInterval.Type, ByTxType.Type<TxCount.Type>]

export type Type = TxCountChartItem

export const codec = T.tuple(TimeInterval.codec, ByTxType.codec(TxCount.codec))
