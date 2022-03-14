import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as ByTxType from "./."

const mockByTxTypeNumber: unknown = {
  trade: 0,
  addLiquidity: 1,
  removeLiquidity: 2,
  total: 3,
}

describe("ByTxType", () => {
  it("Decodes ByTxType", () => {
    const result: E.Either<IO.Errors, ByTxType.Type<number>> = ByTxType.codec(
      IO.number
    ).decode(mockByTxTypeNumber)
    expect(result).toBeRight()
  })
})
