import "@relmify/jest-fp-ts"

import * as E from "fp-ts/Either"

import * as Balances from "."

describe("Balances", () => {
  it("Decodes Balances", async () => {
    const mock = {
      actual: 21914942790.237198,
      nominal: 285978177040.6138,
    }

    const result = Balances.codec.decode(mock)

    if (E.isLeft(result)) {
      console.info(JSON.stringify(result.left, null, 2))
    }

    expect(result).toBeRight()
  })
})
