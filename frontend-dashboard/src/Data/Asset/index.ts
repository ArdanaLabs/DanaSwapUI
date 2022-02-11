import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as string from "fp-ts/string"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export default interface Asset
  extends Newtype.Newtype<{ readonly Asset: unique symbol }, string> {}

export type Type = Asset

export const codec: IO.Type<Asset, string> = fromNewtype<Asset>(IO.string)

export const iso = Newtype.iso<Asset>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq<Asset>(string.Eq)

export const Ord = Newtype.getOrd<Asset>(string.Ord)

export const showAsset: Show_.Show<Asset> = {
  show: (l) => `Asset(${iso.unwrap(l)})`,
}
