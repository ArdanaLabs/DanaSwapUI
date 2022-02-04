import * as Eq_ from "fp-ts/Eq"
import * as Show_ from "fp-ts/Show"
import { fst, snd, mapFst, mapSnd } from "fp-ts/Tuple"
import { Lens } from "monocle-ts/Lens"
import * as IO from "io-ts"
import * as T from "io-ts/Type"
import { DateFromISOString } from "io-ts-types/DateFromISOString"

/*
 * @category model
 */
export const codec = T.tuple(DateFromISOString, DateFromISOString)

export type TimeInterval = IO.TypeOf<typeof codec>

export type Type = TimeInterval

/*
 * @category constructor
 */
export function dateRange(start: Date, end: Date): TimeInterval {
  return [start, end]
}

export const _start: Lens<[Date, Date], Date> = {
  get: fst,
  set: (v) => mapFst(() => v),
}

export const _end: Lens<[Date, Date], Date> = {
  get: snd,
  set: (v) => mapSnd(() => v),
}

/*
 * @category instances
 */
export const Eq: Eq_.Eq<TimeInterval> = {
  equals: ([a, c], [b, d]) => a === b && c === d,
}

export const Show: Show_.Show<TimeInterval> = {
  // TODO: be specific
  show: (r) => JSON.stringify(r),
}
