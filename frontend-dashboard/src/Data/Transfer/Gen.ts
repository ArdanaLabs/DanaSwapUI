import { Arbitrary, date, array, record, tuple } from "fast-check"

import * as Asset from "Data/Asset"
import * as Transfer from "Data/Transfer"
import { AssetQuantity } from "Data/Unit"

import { genAddress } from "Data/Address/Gen"
import { genAsset } from "Data/Asset/Gen"
import { genAssetQuantity } from "Data/Unit/AssetQuantity/Gen"

export const genTransfer: Arbitrary<Transfer.Type> = record({
  counterpartyAddress: genAddress,
  created: date(),
  amounts: array(tuple(genAsset, genAssetQuantity)).map(
    (pairs: [Asset.Type, AssetQuantity.Type][]) => new Map(pairs)
  ),
})
