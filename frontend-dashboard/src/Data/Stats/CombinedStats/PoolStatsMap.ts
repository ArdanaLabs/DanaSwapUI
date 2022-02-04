import * as IO from "io-ts"
import { readonlyMapFromRecord } from "io-ts-utils"

import * as PoolSetName from "Data/Pool/PoolSetName"
import * as PoolStats from "Data/Stats/PoolStats"

export const codec: IO.Type<
  ReadonlyMap<PoolSetName.Type, PoolStats.Type>,
  IO.UnknownRecordC
> = readonlyMapFromRecord(PoolSetName.codec, PoolStats.codec)

export type PoolStatsMap = IO.TypeOf<typeof codec>

export type Type = PoolStatsMap
