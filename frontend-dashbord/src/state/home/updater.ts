import { useEffect } from "react";
import { useDispatch } from "react-redux";

import { AppDispatch } from "state";
import { updateTotalStats } from "./actions";

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>();

  useEffect(() => {
    dispatch(
      updateTotalStats({
        totalDepositsAllPoolsUSD: 20,
        totalDailyVolumeUSD: 30,
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
            liquidityUtilizationRatio: 0,
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
            liquidityUtilizationRatio: 0,
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
            liquidityUtilizationRatio: 0,
          },
        },
      })
    );
    return () => {};
  }, [dispatch]);

  return null;
}
