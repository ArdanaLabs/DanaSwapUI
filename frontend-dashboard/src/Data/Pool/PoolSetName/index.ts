import * as Show_ from "fp-ts/Show"
import * as string from "fp-ts/string"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import * as Newtype from "newtype-ts"

export default interface PoolSetName
  extends Newtype.Newtype<{ readonly PoolSetName: unique symbol }, string> {}

export type Type = PoolSetName

export const codec: IO.Type<PoolSetName, string> = fromNewtype<PoolSetName>(
  IO.string
)

export const iso = Newtype.iso<PoolSetName>()

export const Eq = Newtype.getEq<PoolSetName>(string.Eq)

export const Ord = Newtype.getOrd<PoolSetName>(string.Ord)

export const Show: Show_.Show<PoolSetName> = {
  show: (l) => `PoolSetName(${iso.unwrap(l)})`,
}
