import { useSelector } from "react-redux"
import { AppState } from "state"
import { API_URL } from "config/endpoints"

export function useTotalStats() {
  const {
    totalDepositsAllPoolsUSD,
    totalDailyVolumeUSD,
    totalDailyFeeVolumeUSD,
    totalLiquidityUtilization,
  } = useSelector<AppState, AppState["home"]>((state) => state.home)

  return {
    totalDepositsAllPoolsUSD,
    totalDailyVolumeUSD,
    totalDailyFeeVolumeUSD,
    totalLiquidityUtilization,
  }
}

export function usePoolStats() {
  const { poolStats } = useSelector<AppState, AppState["home"]>(
    (state) => state.home
  )

  return poolStats
}

export async function getStats() {
  try {
    const result = await fetch(API_URL + "/combined")

    return result.json()
  } catch (e) {
    console.log("error fetching", e)
    return null
  }
}
