import * as Show_ from "fp-ts/Show"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import * as Newtype from "newtype-ts"

import * as USD from "Data/Unit/USD"

/*
 * @category model
 */
export default interface Volume
  extends Newtype.Newtype<{ readonly Volume: unique symbol }, USD.Type> {}

export type Type = Volume

export const codec: IO.Type<Volume, USD.Type> = fromNewtype<Volume>(USD.codec)

export const iso = Newtype.iso<Volume>()

/*
 * @category instances
 */
export const Eq = Newtype.getEq(USD.Eq)

export const Ord = Newtype.getOrd(USD.Ord)

export const Show: Show_.Show<Volume> = {
  show: (l) => `Volume(${USD.Show.show(iso.unwrap(l))})`,
}
