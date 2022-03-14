import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import mocks from "../../../../mocks"

import * as CombinedStats from "."

let fileMock: unknown

beforeAll(async () => {
  let data = await mocks.combined()
  if (E.isRight(data)) {
    fileMock = data.right
  } else {
    throw data.left
  }
})

describe("CombinedStats", () => {
  it("Decodes CombinedStats from mock data", async () => {
    const result: E.Either<IO.Errors, CombinedStats.Type> =
      CombinedStats.codec.decode(fileMock)
    if (E.isLeft(result)) {
      console.info(JSON.stringify(result.left, null, 2))
    }
    expect(result).toBeRight()
  })
})
