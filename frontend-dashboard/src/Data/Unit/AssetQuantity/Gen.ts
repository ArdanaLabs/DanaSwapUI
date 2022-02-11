import { Arbitrary, float } from "fast-check"
import * as AssetQuantity from "Data/Unit/AssetQuantity"

export const genAssetQuantity: Arbitrary<AssetQuantity.Type> = float().map(
  AssetQuantity.iso.wrap
)
