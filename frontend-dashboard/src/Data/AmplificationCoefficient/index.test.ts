import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as AmplificationCoefficient from "./."

const mockAmplificationCoefficient: unknown = 69.99

describe("AmplificationCoefficient", () => {
  it("Decodes AmplificationCoefficient", () => {
    const result: E.Either<IO.Errors, AmplificationCoefficient.Type> =
      AmplificationCoefficient.codec.decode(mockAmplificationCoefficient)
    expect(result).toBeRight()
  })
})
