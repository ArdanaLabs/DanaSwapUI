import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as USD from "./."

const mockUSD: unknown = 69.99

describe("USD", () => {
  it("Decodes USD", () => {
    const result: E.Either<IO.Errors, USD.Type> = USD.codec.decode(mockUSD)
    expect(result).toBeRight()
  })
})
