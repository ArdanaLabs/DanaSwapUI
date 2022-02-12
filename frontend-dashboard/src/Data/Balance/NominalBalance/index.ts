import * as Show_ from "fp-ts/Show"
import * as Newtype from "newtype-ts"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

import * as USD from "Data/Unit/USD"

export default interface NominalBalance
  extends Newtype.Newtype<
    { readonly NominalBalance: unique symbol },
    USD.Type
  > {}

export type Type = NominalBalance

export const codec: IO.Type<NominalBalance, USD.Type> =
  fromNewtype<NominalBalance>(USD.codec)

export const iso = Newtype.iso<NominalBalance>()

export const Eq = Newtype.getEq<NominalBalance>(USD.Eq)

export const Ord = Newtype.getOrd<NominalBalance>(USD.Ord)

export const Show: Show_.Show<NominalBalance> = {
  show: (l) => `NominalBalance(${USD.Show.show(iso.unwrap(l))})`,
}
