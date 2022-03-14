import * as IO from "io-ts"
import { readonlyMapFromRecord } from "io-ts-utils"

import * as Asset from "Data/Asset"
import { AssetQuantity } from "Data/Unit"

export const codec: IO.Type<
  ReadonlyMap<Asset.Type, AssetQuantity.Type>,
  IO.UnknownRecordC,
  unknown
> = readonlyMapFromRecord(Asset.codec, AssetQuantity.codec)

export type Amounts = IO.TypeOf<typeof codec>

export type Type = Amounts
