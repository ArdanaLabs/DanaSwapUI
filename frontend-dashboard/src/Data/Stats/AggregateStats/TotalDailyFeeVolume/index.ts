import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import * as Newtype from "newtype-ts"
import { fromNewtype } from "io-ts-types"

import * as USD from "Data/Unit/USD"

/*
 * @category model
 */
export interface TotalDailyFeeVolume
  extends Newtype.Newtype<
    { readonly TotalDailyFeeVolume: unique symbol },
    USD.Type
  > {}

export type Type = TotalDailyFeeVolume

export const codec: IO.Type<TotalDailyFeeVolume, USD.Type> =
  fromNewtype<TotalDailyFeeVolume>(USD.codec)

export const iso = Newtype.iso<TotalDailyFeeVolume>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(USD.Eq)

export const Ord = Newtype.getOrd(USD.Ord)

export const showTotalDailyFeeVolume: Show_.Show<TotalDailyFeeVolume> = {
  show: (l) => `TotalDailyFeeVolume(${USD.Show.show(iso.unwrap(l))})`,
}
