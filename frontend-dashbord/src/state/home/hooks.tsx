import { useSelector } from "react-redux";
import { AppState } from "state";
import { API_URL } from "config/endpoints";

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

export async function getStats() {
  try {
    const result = await fetch(API_URL + "/combined");

    return result.json();
  } catch(e) {
    console.log('error fetching', e);
    return null;
  }
}

export async function getProviderProfits(start: string, end: string) {
  try {
    const result = await fetch(API_URL + `/averageLiquidityProviderProfit?start=${start}&end=${end}`);

    return result.json();
  } catch(e) {
    console.log('error fetching', e);
    return null;
  }
}