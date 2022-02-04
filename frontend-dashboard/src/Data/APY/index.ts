import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

/*
 * @category model
 */
export default interface APY
  extends Newtype.Newtype<{ readonly APY: unique symbol }, number> {}

export type Type = APY

export const iso = Newtype.iso<APY>()

export const codec: IO.Type<APY, number> = fromNewtype<APY>(IO.number)

/*
 * @category instances
 */
export const Eq = Newtype.getEq<APY>(number.Eq)

export const Ord = Newtype.getOrd<APY>(number.Ord)

export const Show: Show_.Show<APY> = {
  show: (l) => `APY(${iso.unwrap(l)})`,
}
