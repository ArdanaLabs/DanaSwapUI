// import * as ReadonlyMap from "fp-ts/ReadonlyMap"
import * as IO from "io-ts"
import { DateFromISOString } from "io-ts-types/DateFromISOString"

import * as Address from "Data/Address"

import * as Amounts from "./Amounts"

export * as Amounts from "./Amounts"

export default interface Transfer {
  readonly counterpartyAddress: Address.Type // TODO: Address Counterparty
  readonly created: Date
  readonly amounts: Amounts.Type
}

export type Type = Transfer

export const codec: IO.Type<Transfer, object> = IO.type(
  {
    counterpartyAddress: Address.codec,
    created: DateFromISOString,
    amounts: Amounts.codec,
  },
  "Transfer"
)
