import { useEffect } from "react";
import { useDispatch } from "react-redux";

import { AppDispatch } from "state";
import { updateTotalStats } from "./actions";
import { getStats } from "./hooks";

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>();

  useEffect(() => {
    const fetchTotalStatsFromEndPoint = async () => {
      const totalStats: any = await getStats();
      console.log("totalStats", totalStats);

      // dispatch(updateTotalStats(totalStats));
    };

    setInterval(async () => {
      await fetchTotalStatsFromEndPoint();
    }, 1000 * 60);

    fetchTotalStatsFromEndPoint();

    dispatch(
      updateTotalStats({
        totalDailyTxCount: 323,
        totalDailyFeeVolumeUSD: 23.33,
        totalDepositsAllPoolsUSD: 9006029010.68,
        totalDailyVolumeUSD: 147471544,
        poolStats: {
          sBTC: {
            recentDailyAPYPercent: 10,
            recentWeeklyAPYPercent: 20,
            recentMonthlyAPYPercent: 30,
            recentAnnualAPYPercent: 40,
            totalAPYPercent: 50,
            recentDailyVolumeUSD: 2.55,
            reserves: {
              DAI: 1,
              USDC: 2,
              USDT: 3,
              sUSD: 4,
            },
            feePercent: 1,
            adminFeePercent: 2,
            virtualPriceUSD: 200,
            liquidityUtilization: 0,
            dailyTxCount: 3,
            dailyFeeVolumeUSD: 1,
            navUSD: 12,
          },
          aBTC: {
            recentDailyAPYPercent: 10,
            recentWeeklyAPYPercent: 20,
            recentMonthlyAPYPercent: 30,
            recentAnnualAPYPercent: 40,
            totalAPYPercent: 50,
            recentDailyVolumeUSD: 255,
            reserves: {
              DAI: 1,
              USDC: 2,
              USDT: 3,
              sUSD: 4,
            },
            feePercent: 1,
            adminFeePercent: 2,
            virtualPriceUSD: 200,
            liquidityUtilization: 0,
            dailyTxCount: 3,
            dailyFeeVolumeUSD: 1,
            navUSD: 12,
          },
          bBTC: {
            recentDailyAPYPercent: 10,
            recentWeeklyAPYPercent: 20,
            recentMonthlyAPYPercent: 30,
            recentAnnualAPYPercent: 40,
            totalAPYPercent: 50,
            recentDailyVolumeUSD: 255,
            reserves: {
              DAI: 10.76,
              USDC: 2.2,
              USDT: 3,
              sUSD: 4,
            },
            feePercent: 1,
            adminFeePercent: 2,
            virtualPriceUSD: 200,
            liquidityUtilization: 0,
            dailyTxCount: 3,
            dailyFeeVolumeUSD: 1,
            navUSD: 12,
          },
        },
      })
    );
    return () => {};
  }, [dispatch]);

  return null;
}
