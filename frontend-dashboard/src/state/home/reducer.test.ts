import reducer, { initialState } from "./reducer"
import { TotalStat, updateTotalStats } from "./actions"

describe("Chart reducers", () => {
  it("should return TotalStat object values when dispatch updateTotalStats action", () => {
    const mock: TotalStat = {
      totalDailyTxCount: null,
      totalDailyFeeVolumeUSD: null,
      totalDailyVolumeUSD: null,
      totalDepositsAllPoolsUSD: null,
      totalLiquidityUtilization: null,
      poolStats: null,
    }

    expect(reducer(initialState, updateTotalStats(mock))).toStrictEqual({
      totalDailyTxCount: mock.totalDailyTxCount,
      totalDailyFeeVolumeUSD: mock.totalDailyFeeVolumeUSD,
      totalDailyVolumeUSD: mock.totalDailyVolumeUSD,
      totalDepositsAllPoolsUSD: mock.totalDepositsAllPoolsUSD,
      totalLiquidityUtilization: mock.totalLiquidityUtilization,
      poolStats: mock.poolStats,
    })
  })
})
