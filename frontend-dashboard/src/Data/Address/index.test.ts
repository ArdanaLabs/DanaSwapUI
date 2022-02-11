import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as Address from "."

const mockAddress: unknown = "Qux"

describe("Address", () => {
  it("Decodes CounterPartyAddress", () => {
    const result: E.Either<IO.Errors, Address.Type> =
      Address.codec.decode(mockAddress)
    expect(result).toBeRight()
  })

  /*
  it("Decodes CounterPartyAddress", () => {
    const result: E.Either<IO.Errors, SubPoolAddress> =
      typeCounterPartyAddress.decode(mockAddress)
    console.log(JSON.stringify(result, null, 2))
    expect(result).toBeRight()
  })

  it("CounterPartyAddress cannot be SubPoolAddress", () => {
    const resultCPA: E.Either<IO.Errors, CounterPartyAddress> =
      typeCounterPartyAddress.decode(mockAddress)
    const resultSPA: E.Either<IO.Errors, SubPoolAddress> =
      typeSubPoolAddress.decode(mockAddress)
    expect(resultCPA).toBeRight()
    expect(resultSPA).toBeRight()
  })
  */
})
