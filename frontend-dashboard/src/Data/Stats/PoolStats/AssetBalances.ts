import * as IO from "io-ts"
import { readonlyMapFromRecord } from "io-ts-utils"

import * as Asset from "Data/Asset"
import * as Balances from "Data/Balance"

export const codec = readonlyMapFromRecord(Asset.codec, Balances.codec)

export type AssetBalances = IO.TypeOf<typeof codec>
