import * as IO from "io-ts"
import { DateFromISOString } from "io-ts-types/DateFromISOString"

import * as Address from "Data/Address"
import * as Asset from "Data/Asset"
import { AssetQuantity } from "Data/Unit"

export const codec = IO.strict(
  {
    spentAsset: Asset.codec,
    purchasedAsset: Asset.codec,
    spentAmount: AssetQuantity.codec,
    purchasedAmount: AssetQuantity.codec,
    created: DateFromISOString,
    counterpartyAddress: Address.codec, // TODO: Address Counterparty
  },
  "Trade"
)

export type Trade = IO.TypeOf<typeof codec>

export type Type = Trade
