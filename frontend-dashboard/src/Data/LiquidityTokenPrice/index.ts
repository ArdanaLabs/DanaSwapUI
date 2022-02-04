import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import { USD } from "Data/Unit"

/*
 * @category model
 */
export default interface LiquidityTokenPrice
  extends Newtype.Newtype<
    { readonly LiquidityTokenPrice: unique symbol },
    USD.Type
  > {}

export type Type = LiquidityTokenPrice

export const iso = Newtype.iso<LiquidityTokenPrice>()

export const codec: IO.Type<LiquidityTokenPrice, USD.Type> =
  fromNewtype<LiquidityTokenPrice>(USD.codec)

/*
 * @category instances
 */
export const Eq = Newtype.getEq<LiquidityTokenPrice>(USD.Eq)

export const Ord = Newtype.getOrd<LiquidityTokenPrice>(USD.Ord)

export const Show: Show_.Show<LiquidityTokenPrice> = {
  show: (l) => `LiquidityTokenPrice(${USD.Show.show(iso.unwrap(l))})`,
}
