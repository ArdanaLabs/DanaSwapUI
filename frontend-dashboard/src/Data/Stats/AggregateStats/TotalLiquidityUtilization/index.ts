import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import * as Newtype from "newtype-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export interface TotalLiquidityUtilization
  extends Newtype.Newtype<
    { readonly TotalLiquidityUtilization: unique symbol },
    number
  > {}

export type Type = TotalLiquidityUtilization

export const codec: IO.Type<TotalLiquidityUtilization, number> =
  fromNewtype<TotalLiquidityUtilization>(IO.number)

export const iso = Newtype.iso<TotalLiquidityUtilization>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(number.Eq)

export const Ord = Newtype.getOrd(number.Ord)

export const Show: Show_.Show<TotalLiquidityUtilization> = {
  show: (l) => `TotalLiquidityUtilization(${iso.unwrap(l)})`,
}
