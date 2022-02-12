import { testProp, fc } from "jest-fast-check"
import * as O from "fp-ts/Option"
import * as NEA from "fp-ts/NonEmptyArray"

import { TimeInterval } from "Data/TimeInterval"
import { genTimeInterval } from "Data/TimeInterval/Gen"

import { extractDateAxis } from "./extractor"

describe("Hooks extractor.ts", () => {
  describe("extractDateAxis", () => {
    describe("basic", () => {
      const mock: NEA.NonEmptyArray<[TimeInterval, unknown]> = [
        [
          [new Date("2020-05-25T13:20"), new Date("2020-05-25T14:25")],
          undefined,
        ],
        [
          [new Date("2021-04-11T19:01"), new Date("2021-04-11T20:18")],
          undefined,
        ],
      ]
      const result: NEA.NonEmptyArray<Date> = extractDateAxis(mock)
      it("should have 3 elements", () => {
        expect(result.length).toEqual(3)
      })
    })

    testProp(
      "Should have a length + 1 of input",
      [fc.array(fc.tuple(genTimeInterval, fc.constant(undefined)))],
      (x: [TimeInterval, unknown][]): boolean => {
        return O.fold(
          (): boolean => true,
          (nea: NEA.NonEmptyArray<[TimeInterval, unknown]>): boolean =>
            nea.length + 1 === extractDateAxis(nea).length
        )(NEA.fromArray(x))
      }
    )
  })
})
