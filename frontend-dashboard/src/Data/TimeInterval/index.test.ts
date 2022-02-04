// import { itProp, fc } from "jest-fast-check"
import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as TimeInterval from "."

const mockTimeInterval: unknown = [
  "2020-12-12T00:00:00Z",
  "2020-12-12T00:00:01Z",
]

describe("TimeInterval", () => {
  it("Decodes TimeInterval", () => {
    const result: E.Either<IO.Errors, TimeInterval.Type> =
      TimeInterval.codec.decode(mockTimeInterval)
    expect(result).toBeRight()
  })
})
