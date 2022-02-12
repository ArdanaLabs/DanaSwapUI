import * as Show_ from "fp-ts/Show"
import * as Newtype from "newtype-ts"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

export default interface AmplificationCoefficient
  extends Newtype.Newtype<
    { readonly AmplificationCoefficient: unique symbol },
    number
  > {}

export type Type = AmplificationCoefficient

export const codec: IO.Type<AmplificationCoefficient, number> =
  fromNewtype<AmplificationCoefficient>(IO.number)

export const iso = Newtype.iso<AmplificationCoefficient>()

export const Eq = Newtype.getEq<AmplificationCoefficient>(number.Eq)

export const Ord = Newtype.getOrd<AmplificationCoefficient>(number.Ord)

export const Show: Show_.Show<AmplificationCoefficient> = {
  show: (l) => `AmplificationCoefficient(${iso.unwrap(l)})`,
}
