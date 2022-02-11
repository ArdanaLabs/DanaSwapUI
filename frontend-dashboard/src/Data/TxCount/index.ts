import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export default interface TxCount
  extends Newtype.Newtype<{ readonly TxCount: unique symbol }, number> {}

export type Type = TxCount

// TODO: validate, natural
export const codec: IO.Type<TxCount, number> = fromNewtype<TxCount>(IO.number)

export const iso = Newtype.iso<TxCount>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(number.Eq)

export const Ord = Newtype.getOrd(number.Ord)

export const Show: Show_.Show<TxCount> = {
  show: (l) => `TxCount(${iso.unwrap(l)})`,
}
