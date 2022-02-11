// <abbr title="net asset value">NAV</abbr>
import { Newtype, iso } from "newtype-ts"
import { Eq } from "fp-ts/Eq"
import { Show } from "fp-ts/Show"
import * as IO from "io-ts"

export interface NAV<A> extends Newtype<{ readonly NAV: unique symbol }, A> {}

export function isoNAV<A>(a: A) {
  return iso<NAV<A>>()
}

//export function eqNAV<A>(E: Eq<A>): Eq<NAV<A>> {
//  return {
//    equals: (a: NAV<A>, b: NAV<A>) =>
//      E.equals(isoNAV.unwrap(a), isoNAV.unwrap(b)),
//  }
//}

//export function ordNAV<A>(O: Ord<A>): Eq<NAV<A>> {
//  return {
//    equals: (a, b) => E.equals(isoNAV.unwrap(a), isoNAV.unwrap(b)),
//  }
//}

//export function showNAV<A>(S: Show<A>): Show<NAV<A>> {
//  return {
//    show: (n: Show<A>) => `NAV(${S.show(isoNAV.unwrap(n))})`,
//  }
//}

//export const codec = new IO.Type<Number, number, unknown>(
// "NAV",
// (u: unknown): u is NAV => typeof u === "number",
// (input, context) =>
//   typeof input === "number" ? IO.success(input) : IO.failure(input, context),
// isoNAV.wrap
//)
