import * as Eq_ from "fp-ts/Eq"

/**
 * @category model
 */
export enum Granularity {
  OneMinute = "OneMinute",
  FiveMinutes = "FiveMinutes",
  TenMinutes = "TenMinutes",
  ThirtyMinutes = "ThirtyMinutes",
  OneHour = "OneHour",
  FourHours = "FourHours",
  TwelveHours = "TwelveHours",
  OneDay = "OneDay",
  OneWeek = "OneWeek",
  OneMonth = "OneMonth",
}

/**
 * @category instances
 */
export const Eq: Eq_.Eq<Granularity> = {
  equals: (a: Granularity, b: Granularity) => a === b,
}
