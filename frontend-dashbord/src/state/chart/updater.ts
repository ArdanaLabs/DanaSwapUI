import { useEffect } from "react";
import { useDispatch } from "react-redux";

import { AppDispatch } from "state";
import { 
  updateAggVolume,
  updateAggLiquidity,
  updatePoolFees,
  updatePoolVolume,
  updatePoolLiquidity,
  updatePoolTxCount,
  updatePoolAPY,
  updatePoolTransactions
} from "./actions";
import {
  getAggVolume,
  getAggLiquidity,
  getPoolFees,
  getPoolVolume,
  getPoolLiquidity,
  getPoolTXCount,
  getPoolAPY,
  getPoolTransactions,
} from "./hooks";
import { FiveMinutes, OneDay, OneWeek } from "config/grains";
import { Any } from "config/txTypes";

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>();

  useEffect(() => {
    const fetchAggVolume = async () => {
      const aggVolume: any = await getAggVolume(
        "2020-12-12T00:00:00.0Z",
        "2020-12-12T00:05:00.0Z",
        FiveMinutes
      );
      console.log("aggVolume", aggVolume);
      if (!aggVolume) return;
      const params = aggVolume.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          addLiquidity: volume[1].addLiquidity,
          removeLiquidity: volume[1].removeLiquidity,
          total: volume[1].total,
          trade: volume[1].trade,
        }
      });
      dispatch(updateAggVolume(params));
    };
    const fetchAggLiquidity = async () => {
      const aggLiquidity: any = await getAggLiquidity(
        "2020-12-12T00:00:00.0Z",
        "2020-12-14T00:00:00.0Z",
        OneDay
      );
      console.log("aggLiquidity", aggLiquidity);
      if (!aggLiquidity) return;
      const params = aggLiquidity.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          value: volume[1],
        }
      });
      dispatch(updateAggLiquidity(params));
    };
    const fetchPoolFees = async () => {
      const poolFees: any = await getPoolFees(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolFees", poolFees);
      if (!poolFees) return;
      const params = poolFees.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          value: volume[1],
        }
      });
      dispatch(updatePoolFees(params));
    };
    const fetchPoolVolume = async () => {
      const poolVolume: any = await getPoolVolume(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolVolume", poolVolume);
      if (!poolVolume) return;
      const params = poolVolume.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          addLiquidity: volume[1].addLiquidity,
          removeLiquidity: volume[1].removeLiquidity,
          total: volume[1].total,
          trade: volume[1].trade,
        }
      });
      dispatch(updatePoolVolume(params));
    };
    const fetchPoolLiquidity = async () => {
      const poolLiquidity: any = await getPoolLiquidity(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolLiquidity", poolLiquidity);
      if (!poolLiquidity) return;
      const params = poolLiquidity.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          value: volume[1],
        }
      });
      dispatch(updatePoolLiquidity(params));
    };
    const fetchPoolTxCount = async () => {
      const poolTXCount: any = await getPoolTXCount(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolTXCount", poolTXCount);
      if (!poolTXCount) return;
      const params = poolTXCount.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          addLiquidity: volume[1].addLiquidity,
          removeLiquidity: volume[1].removeLiquidity,
          total: volume[1].total,
          trade: volume[1].trade,
        }
      });
      dispatch(updatePoolTxCount(params));
    };
    const fetchPoolAPY = async () => {
      const poolAPY: any = await getPoolAPY(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolAPY", poolAPY);
      if (!poolAPY) return;
      const params = poolAPY.map((volume: any) => {
        return {
          start: volume[0][0],
          end: volume[0][1],
          value: volume[1],
        }
      });
      dispatch(updatePoolAPY(params));
    };
    const fetchPoolTransactions = async () => {
      const poolTransactions: any = await getPoolTransactions(
        "foo",
        0,
        10,
        Any
      );
      console.log("poolTransactions", poolTransactions);
      poolTransactions && dispatch(updatePoolTransactions(poolTransactions));
    };

    fetchAggVolume();
    fetchAggLiquidity();
    fetchPoolFees();
    fetchPoolVolume();
    fetchPoolLiquidity();
    fetchPoolTxCount();
    fetchPoolAPY();
    fetchPoolTransactions();
    return () => {};
  }, [dispatch]);

  return null;
}
