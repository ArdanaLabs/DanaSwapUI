import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import * as Newtype from "newtype-ts"

import * as USD from "Data/Unit/USD"

export default interface ActualBalance
  extends Newtype.Newtype<
    { readonly ActualBalance: unique symbol },
    USD.Type
  > {}

export type Type = ActualBalance

export const codec: IO.Type<ActualBalance, USD.Type> =
  fromNewtype<ActualBalance>(USD.codec)

export const iso = Newtype.iso<ActualBalance>()

export const Eq = Newtype.getEq<ActualBalance>(USD.Eq)

export const Ord = Newtype.getOrd<ActualBalance>(USD.Ord)

export const Show: Show_.Show<ActualBalance> = {
  show: (l) => `ActualBalance(${USD.Show.show(iso.unwrap(l))})`,
}
