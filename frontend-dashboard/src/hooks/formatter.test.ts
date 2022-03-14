import { USD } from "Data/Unit"

import { printCurrencyUSD } from "hooks"

describe("Hooks formatter", () => {
  describe("printCurrencyUSD method", () => {
    it("default format", () => {
      const result = printCurrencyUSD(USD.iso.wrap(2_456_987))
      expect(result).toBe("$2.46M")
    })

    it("should return $ x.**K", () => {
      const result = printCurrencyUSD(USD.iso.wrap(1_323), {
        minimumFractionDigits: 2,
      })
      expect(result).toBe("$1.32K")
    })

    it("should return default million", () => {
      const result = printCurrencyUSD(USD.iso.wrap(1_000_323))
      const expected = "$1M"
      expect(result).toBe(expected)
    })

    it("should return $ x.**M", () => {
      const result = printCurrencyUSD(USD.iso.wrap(1_000_323), {
        minimumFractionDigits: 2,
      })
      const expected = "$1.00M"
      expect(result).toBe(expected)
    })

    it("should return $ x.**B", () => {
      const result = printCurrencyUSD(USD.iso.wrap(1_456_323_435), {
        minimumFractionDigits: 2,
      })
      expect(result).toBe("$1.46B")
    })

    it("should return $ x.**T", () => {
      const result = printCurrencyUSD(USD.iso.wrap(1_876_789_456_323), {
        minimumFractionDigits: 2,
      })
      expect(result).toBe("$1.88T")
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
