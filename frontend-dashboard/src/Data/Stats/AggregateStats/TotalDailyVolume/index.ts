import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

import * as USD from "Data/Unit/USD"

/*
 * @category model
 */
export interface TotalDailyVolume
  extends Newtype.Newtype<
    { readonly TotalDailyVolume: unique symbol },
    USD.Type
  > {}

export type Type = TotalDailyVolume

export const codec: IO.Type<TotalDailyVolume, USD.Type> =
  fromNewtype<TotalDailyVolume>(USD.codec)

export const iso = Newtype.iso<TotalDailyVolume>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(USD.Eq)

export const Ord = Newtype.getOrd(USD.Ord)

export const Show: Show_.Show<TotalDailyVolume> = {
  show: (l) => `TotalDailyVolume(${USD.Show.show(iso.unwrap(l))})`,
}
