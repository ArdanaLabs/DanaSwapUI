import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import mocks from "../../../../mocks"

import * as PoolStats from "."

let fileMock: unknown

beforeAll(async () => {
  const data = await mocks.combined()
  if (E.isRight(data)) {
    fileMock = data.right
  } else {
    throw data.left
  }
})

describe("PoolStats", () => {
  it("Decodes PoolStats from combined mock data", async () => {
    const { poolStats } = fileMock as any
    const k: string = Object.keys(poolStats)[0]
    const result: E.Either<IO.Errors, PoolStats.Type> = PoolStats.codec.decode(
      poolStats[k]
    )
    if (E.isLeft(result)) {
      console.warn(JSON.stringify(result.left, null, 2))
    }
    expect(result).toBeRight()
  })
})
