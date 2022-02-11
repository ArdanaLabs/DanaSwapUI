import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export interface TotalDailyTxCount
  extends Newtype.Newtype<
    { readonly TotalDailyTxCount: unique symbol },
    number
  > {}

export type Type = TotalDailyTxCount

export const codec: IO.Type<TotalDailyTxCount, number> =
  fromNewtype<TotalDailyTxCount>(IO.number)

export const iso = Newtype.iso<TotalDailyTxCount>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(number.Eq)

export const Ord = Newtype.getOrd(number.Ord)

export const Show: Show_.Show<TotalDailyTxCount> = {
  show: (l) => `TotalDailyTxCount(${iso.unwrap(l) | 0})`,
}
