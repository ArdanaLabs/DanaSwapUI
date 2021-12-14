import * as Option from "fp-ts/Option"
import { getOption } from "fp-ts-laws/lib/Option"
import { itProp, fc } from "jest-fast-check"
import { RangedLiquidity, RangedVolume } from "state/chart/actions"
import { extractXAxis, extractYAxis, findKeyFromObject } from "./extractor"

const mock: RangedVolume[] = [
  {
    start: Option.some("1"),
    end: Option.some("2"),
    addLiquidity: Option.none,
    removeLiquidity: Option.none,
    total: Option.some(23),
    trade: Option.none,
  },
  {
    start: Option.some("2"),
    end: Option.some("3"),
    addLiquidity: Option.none,
    removeLiquidity: Option.none,
    total: Option.some(56),
    trade: Option.none,
  },
]

const mockObject = {
  foo: {
    bar: 1,
  },
}

const RangedVolumeType = fc.record({
  start: getOption(fc.string()),
  end: getOption(fc.string()),
  addLiquidity: getOption(fc.string()),
  removeLiquidity: getOption(fc.string()),
  total: getOption(fc.string()),
  trade: getOption(fc.string()),
}) as fc.Arbitrary<RangedVolume>

const RangedLiquidityType = fc.record({
  start: getOption(fc.string()),
  end: getOption(fc.string()),
  value: getOption(fc.integer()),
}) as fc.Arbitrary<RangedLiquidity>

describe("Hooks extractor.ts", () => {
  describe("extractXAxis method", () => {
    const newAxis = extractXAxis(mock)

    it("should return string array type", () => {
      expect(newAxis).toBeInstanceOf(Array)
    })

    it("should return expected string array", () => {
      const expected = [Option.some("1"), Option.some("2"), Option.some("3")]
      expect(newAxis).toEqual(expected)
    })

    itProp(
      "should check param property",
      [fc.oneof(fc.array(RangedVolumeType), fc.array(RangedLiquidityType))],
      (arg0) => {
        extractXAxis(arg0) // TODO: this seems broken
        return true
      }
    )
  })

  describe("extractYAxis method", () => {
    const newAxis = extractYAxis(mock, "total")

    it("should return string array type", () => {
      expect(newAxis).toBeInstanceOf(Array)
    })

    it("should return expected string array", () => {
      const expected = [Option.some(23), Option.some(56)]
      expect(newAxis).toEqual(expected)
    })

    itProp(
      "should check param property",
      [
        fc.oneof(fc.array(RangedVolumeType), fc.array(RangedLiquidityType)),
        fc.string(),
      ],
      (arg0, arg1) => {
        return Array.isArray(extractYAxis(arg0, arg1))
      }
    )
  })

  describe("findKeyFromObject method", () => {
    const result = findKeyFromObject(mockObject, "bar")

    it("should return expected number", () => {
      const expected = 1
      expect(result).toEqual(expected)
    })

    // TODO: broken
    //itProp(
    //  "should check param property",
    //  [fc.json(), fc.string()],
    //  (arg0, arg1) => {
    //    findKeyFromObject(arg0, arg1)
    //    return true
    //  }
    //)
  })
})
