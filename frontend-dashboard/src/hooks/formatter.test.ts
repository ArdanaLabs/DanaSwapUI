import { printCurrencyUSD } from "hooks"

describe("Hooks formatter", () => {
  describe("printCurrencyUSD method", () => {
    it("should return $ 0", () => {
      const result = printCurrencyUSD(null, 0)
      const expected = "$0"
      expect(result).toBe(expected)
    })
    it("should return $0", () => {
      const result = printCurrencyUSD(null, 0)
      const expected = "$0"
      expect(result).toBe(expected)
    })
    it("should return $ x.**K", () => {
      const result = printCurrencyUSD(1323, 2)
      const expected = "$1.32K"
      expect(result).toBe(expected)
    })
    it("should return $ x.**M", () => {
      const result = printCurrencyUSD(1456323, 2)
      const expected = "$1.46M"
      expect(result).toBe(expected)
    })
    it("should return $ x.**B", () => {
      const result = printCurrencyUSD(1456323435, 2)
      const expected = "$1.46B"
      expect(result).toBe(expected)
    })
    it("should return $ x.**T", () => {
      const result = printCurrencyUSD(1876789456323, 2)
      const expected = "$1.88T"
      expect(result).toBe(expected)
    })

    // Since the function comes from Intl, it likely isn’t worth property testing
    /*
    itProp(
      "should check the properties of printCurrencyUSD method",
      [fc.nat(), fc.nat()],
      (num, digits) => {
        const formattedCurrency = printCurrencyUSD(num, digits)
        console.log(formattedCurrency, nReader(formattedCurrency, digits))
        // console.log("$ 13K", nReader("$13K", digits, space))
        return nReader(formattedCurrency, digits) === num
        // return true;
      }
    )
    */
  })
})
