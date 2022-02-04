import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

import * as Percent from "Data/Unit/Percent"

/*
 * @category model
 */
export default interface FeeVolume
  extends Newtype.Newtype<
    { readonly FeeVolume: unique symbol },
    Percent.Type
  > {}

export type Type = FeeVolume

export const codec: IO.Type<FeeVolume, Percent.Type> = fromNewtype<FeeVolume>(
  Percent.codec
)
// TODO: include better validation; withValidate overrides the default
//export const Type: IO.Type<FeeVolume, number, unknown> = withValidate(
//  T,
//  (input: number, context: IO.Context): IO.Validation<FeeVolume> =>
//    map(
//      T.validate(input, context),
//      (n: number) => typeof n === "number" && n >= 0 && n <= 1
//    )
//)

export const iso = Newtype.iso<FeeVolume>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq<FeeVolume>(Percent.Eq)

export const Ord = Newtype.getOrd<FeeVolume>(Percent.Ord)

export const Show: Show_.Show<FeeVolume> = {
  show: (l) => `FeeVolume(${Percent.Show.show(iso.unwrap(l))})`,
}
