import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

// TODO: probably don’t want to rely on IEEE 754 for currency, but it’s fast
// and what the back-end is doing

export interface USD
  extends Newtype.Newtype<{ readonly USD: unique symbol }, number> {}

export type Type = USD

export const codec: IO.Type<USD, number> = fromNewtype<USD>(IO.number)

export const iso = Newtype.iso<Type>()

export const Eq = Newtype.getEq<USD>(number.Eq)

export const Ord = Newtype.getOrd<USD>(number.Ord)

export const Show: Show_.Show<USD> = {
  show: (l) => `USD(${iso.unwrap(l)})`,
}
