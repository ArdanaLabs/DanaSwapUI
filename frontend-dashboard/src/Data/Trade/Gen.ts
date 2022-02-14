import { Arbitrary, date, record } from "fast-check"
import { Trade } from "Data/Trade"

import { genAddress } from "Data/Address/Gen"
import { genAsset } from "Data/Asset/Gen"
import { genAssetQuantity } from "Data/Unit/Gen"

export const genTrade: Arbitrary<Trade> = record({
  spentAsset: genAsset,
  purchasedAsset: genAsset,
  spentAmount: genAssetQuantity,
  purchasedAmount: genAssetQuantity,
  created: date(),
  counterpartyAddress: genAddress,
})
