import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import * as Newtype from "newtype-ts"
import { fromNewtype } from "io-ts-types"

import * as USD from "Data/Unit/USD"

/*
 * @category model
 */
export interface TotalLiquidityUtilization
  extends Newtype.Newtype<
    { readonly TotalLiquidityUtilization: unique symbol },
    USD.Type
  > {}

export type Type = TotalLiquidityUtilization

export const codec: IO.Type<TotalLiquidityUtilization, USD.Type> =
  fromNewtype<TotalLiquidityUtilization>(USD.codec)

export const iso = Newtype.iso<TotalLiquidityUtilization>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(number.Eq)

export const Ord = Newtype.getOrd(number.Ord)

export const Show: Show_.Show<TotalLiquidityUtilization> = {
  show: (l) => `TotalLiquidityUtilization(${iso.unwrap(l)})`,
}
