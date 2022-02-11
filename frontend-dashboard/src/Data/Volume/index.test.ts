import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as Volume from "."

const mockVolume: unknown = 69.99

describe("Volume", () => {
  it("Decodes Volume", () => {
    const result: E.Either<IO.Errors, Volume.Type> =
      Volume.codec.decode(mockVolume)
    expect(result).toBeRight()
  })
})
