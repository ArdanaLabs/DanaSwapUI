import { Arbitrary, oneof, constant } from "fast-check"
import { Granularity } from "Data/Chart/Granularity"

export const genGranularity: Arbitrary<Granularity> = oneof(
  constant(Granularity.OneMinute),
  constant(Granularity.FiveMinutes),
  constant(Granularity.TenMinutes),
  constant(Granularity.ThirtyMinutes),
  constant(Granularity.OneHour),
  constant(Granularity.FourHours),
  constant(Granularity.TwelveHours),
  constant(Granularity.OneDay),
  constant(Granularity.OneWeek),
  constant(Granularity.OneMonth)
)
