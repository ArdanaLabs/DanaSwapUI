// import { itProp, fc } from "jest-fast-check"
import "@relmify/jest-fp-ts"

import * as IO from "io-ts"
import * as E from "fp-ts/Either"

import * as VolumeChart from "."

const mockVolumeChartItem: unknown = [
  ["2020-12-12T00:00:00Z", "2020-12-12T00:00:01Z"],
  {
    addLiquidity: 8.43851032643932e10,
    total: 2.22972322020332e11,
    trade: 1.057337225817773e11,
    removeLiquidity: 2.357361987885822e11,
  },
]

describe("VolumeChart", () => {
  it("Decodes VolumeChartItem", () => {
    const result: E.Either<IO.Errors, VolumeChart.Item.Type> =
      VolumeChart.Item.codec.decode(mockVolumeChartItem)
    expect(result).toBeRight()
  })

  it("Decodes VolumeChart", () => {
    const result: E.Either<IO.Errors, VolumeChart.Type> =
      VolumeChart.codec.decode([mockVolumeChartItem])
    expect(result).toBeRight()
  })
})
