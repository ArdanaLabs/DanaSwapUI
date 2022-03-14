import { Arbitrary, date, tuple } from "fast-check"
import { TimeInterval } from "Data/TimeInterval"

export const genTimeInterval: Arbitrary<TimeInterval> = tuple(
  date(),
  date()
).map((t) => {
  // sort assures that that the start date comes before the end date
  return t.sort((a: Date, b: Date) => a.valueOf() - b.valueOf())
})
