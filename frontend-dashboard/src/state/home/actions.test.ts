import { TotalStat, updateTotalStats } from "./actions"

describe("Home actions", () => {
  describe("updateTotalStats", () => {
    const mock: TotalStat = {
      totalDailyTxCount: null,
      totalDailyFeeVolumeUSD: null,
      totalDailyVolumeUSD: null,
      totalDepositsAllPoolsUSD: null,
      totalLiquidityUtilization: null,
      poolStats: null,
    }

    it("should create action with passed data", () => {
      expect(updateTotalStats(mock)).toEqual({
        type: "home/updateTotalStats",
        payload: mock,
      })
    })
  })
})
