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
} from "./actions";
import {
  getAggVolume,
  getAggLiquidity,
  getPoolFees,
  getPoolVolume,
  getPoolLiquidity,
  getPoolTXCount,
} from "./hooks";
import { FiveMinutes, OneDay, OneWeek } from "config/grains";

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

    fetchAggVolume();
    fetchAggLiquidity();
    fetchPoolFees();
    fetchPoolVolume();
    fetchPoolLiquidity();
    fetchPoolTxCount();
    return () => {};
  }, [dispatch]);

  return null;
}
