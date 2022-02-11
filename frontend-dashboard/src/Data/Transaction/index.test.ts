import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import mocks from "../../../mocks"

import * as Transaction from "."

describe("Transaction", () => {
  it("Decodes Transactions from mock data", async () => {
    const data = await mocks.transactionsPool()
    if (E.isRight(data)) {
      const result: E.Either<IO.Errors, Transaction.WithTotalValue[]> =
        IO.array(Transaction.withTotalValueAdaptedFromAeson).decode(data.right)
      if (E.isLeft(result)) {
        console.info(JSON.stringify(result.left, null, 2))
      }
      expect(result).toBeRight()
    } else {
      console.error("Reading error", data.left)
    }
  })
})
