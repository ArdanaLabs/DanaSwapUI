import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export interface AssetQuantity
  extends Newtype.Newtype<{ readonly AssetQuantity: unique symbol }, number> {}

export type Type = AssetQuantity

export const codec: IO.Type<AssetQuantity, number> = fromNewtype<AssetQuantity>(
  IO.number
)

// instance Validity AssetQuantity where
// validate quantity = mconcat
//   [ genericValidate quantity
//   , declare "Asset quantities are non-negative" $ unAssetQuantity quantity >= 0
//   ]

export const iso = Newtype.iso<AssetQuantity>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq<AssetQuantity>(number.Eq)

export const Ord = Newtype.getOrd<AssetQuantity>(number.Ord)

export const Show: Show_.Show<AssetQuantity> = {
  show: (a) => `AssetQuantity(${iso.unwrap(a)})`,
}
