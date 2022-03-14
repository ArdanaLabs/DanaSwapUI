import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import * as Newtype from "newtype-ts"

import * as USD from "Data/Unit/USD"

/*
 * @category model
 */
export interface TotalDeposits
  extends Newtype.Newtype<
    { readonly TotalDeposits: unique symbol },
    USD.Type
  > {}

export type Type = TotalDeposits

export const codec: IO.Type<TotalDeposits, USD.Type> =
  fromNewtype<TotalDeposits>(USD.codec)

export const iso = Newtype.iso<TotalDeposits>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(USD.Eq)

export const Ord = Newtype.getOrd(USD.Ord)

export const Show: Show_.Show<TotalDeposits> = {
  show: (l) => `TotalDeposits(${USD.Show.show(iso.unwrap(l))})`,
}
