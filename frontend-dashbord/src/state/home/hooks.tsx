import { useSelector } from "react-redux";

import { AppState } from "state";

export function useTotalStats() {
  const { totalDepositsAllPoolsUSD, totalDailyVolumeUSD } = useSelector<
    AppState,
    AppState["home"]
  >((state) => state.home);

  return { totalDepositsAllPoolsUSD, totalDailyVolumeUSD };
}

export function usePoolStats() {
  const { poolStats } = useSelector<
    AppState,
    AppState["home"]
  >((state) => state.home);

  return poolStats;
}