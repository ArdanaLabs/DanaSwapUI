import jsc from "jsverify"
import { nFormatter, nReader } from "hooks"

describe("Hooks formatter", () => {
  describe("nFormatter method", () => {
    it("should return $ 0", () => {
      const result = nFormatter(null, 0)
      const expected = "$ 0"
      expect(result).toBe(expected)
    })
    it("should return $0", () => {
      const result = nFormatter(null, 0, false)
      const expected = "$0"
      expect(result).toBe(expected)
    })
    it("should return $ x.**K", () => {
      const result = nFormatter(1323, 2)
      const expected = "$ 1.32K"
      expect(result).toBe(expected)
    })
    it("should return $ x.**M", () => {
      const result = nFormatter(1456323, 2)
      const expected = "$ 1.46M"
      expect(result).toBe(expected)
    })
    it("should return $ x.**G", () => {
      const result = nFormatter(1789456323, 2)
      const expected = "$ 1.79G"
      expect(result).toBe(expected)
    })
    it("should return $ x.**T", () => {
      const result = nFormatter(1876789456323, 2)
      const expected = "$ 1.88T"
      expect(result).toBe(expected)
    })
    it("should return $ x.**P", () => {
      const result = nFormatter(1765876789456323, 2)
      const expected = "$ 1.77P"
      expect(result).toBe(expected)
    })
    it("should return $ x.**E", () => {
      const result = nFormatter(1543765876789456323, 2)
      const expected = "$ 1.54E"
      expect(result).toBe(expected)
    })

    jsc.property(
      "should check the properties of nFormatter method",
      jsc.nat,
      jsc.nat,
      jsc.bool,
      (num, digits, space) => {
        const formattedCurrency = nFormatter(num, digits, space)
        // console.log(formattedCurrency, nReader(formattedCurrency, digits, space))
        // console.log("$ 13K", nReader("$ 13K", digits, space))
        return nReader(formattedCurrency, digits, space) === num
        // return true;
      }
    )
  })
})
