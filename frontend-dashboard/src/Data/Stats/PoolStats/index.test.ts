import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import mocks from "../../../../mocks"

import * as PoolStats from "."

describe("PoolStats", () => {
  it("Decodes PoolStats from combined mock data", async () => {
    const data = await mocks.combined()
    if (E.isRight(data)) {
      const { poolStats } = data.right as any
      const k: string = Object.keys(poolStats)[0]
      const result: E.Either<IO.Errors, PoolStats.Type> =
        PoolStats.codec.decode(poolStats[k])
      if (E.isLeft(result)) {
        console.info(JSON.stringify(result.left, null, 2))
      }
      expect(result).toBeRight()
    } else {
      console.error(data.left)
    }
  })
})
