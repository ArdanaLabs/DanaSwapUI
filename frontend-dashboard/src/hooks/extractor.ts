import * as NEA from "fp-ts/NonEmptyArray"
import * as T from "fp-ts/Tuple"
import * as TimeInterval from "Data/TimeInterval"

export function extractDateAxis(
  xs: NEA.NonEmptyArray<[TimeInterval.TimeInterval, unknown]>
): NEA.NonEmptyArray<Date> {
  const headStart: Date = TimeInterval._start.get(T.fst(NEA.head(xs)))
  const ends: Date[] = xs.map((t) => TimeInterval._end.get(T.fst(t)))
  return [headStart, ...ends]
}
