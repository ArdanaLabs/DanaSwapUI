import * as Show_ from "fp-ts/Show"
import * as string from "fp-ts/string"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"
import * as Newtype from "newtype-ts"

export default interface Address
  extends Newtype.Newtype<{ readonly Address: unique symbol }, string> {}

export type Type = Address

export const codec: IO.Type<Address, string> = fromNewtype<Address>(IO.string)

export const iso = Newtype.iso<Address>()

export const Eq = Newtype.getEq<Address>(string.Eq)

export const Ord = Newtype.getOrd<Address>(string.Ord)

export const Show: Show_.Show<Address> = {
  show: (l) => `Address(${iso.unwrap(l)})`,
}

/*
export type CounterParty = { _tag: "CounterParty" }
export type SubPool = { _tag: "SubPool" }

export type CounterPartyAddress = Address<CounterParty>
export type SubPoolAddress = Address<SubPool>
*/
