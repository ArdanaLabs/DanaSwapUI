import { useEffect } from "react";
import { useDispatch } from "react-redux";

import { AppDispatch } from "state";
// import { updateAggVol } from "./actions";
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
    const fetchFromEndPoint = async () => {
      const aggVolume: any = await getAggVolume(
        "2020-12-12T00:00:00.0Z",
        "2020-12-12T00:05:00.0Z",
        FiveMinutes
      );
      console.log("aggVolume", aggVolume);

      const aggLiquidity: any = await getAggLiquidity(
        "2020-12-12T00:00:00.0Z",
        "2020-12-14T00:00:00.0Z",
        OneDay
      );
      console.log("aggLiquidity", aggLiquidity);

      const poolFees: any = await getPoolFees(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolFees", poolFees);

      const poolVolume: any = await getPoolVolume(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolVolume", poolVolume);

      const poolLiquidity: any = await getPoolLiquidity(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolLiquidity", poolLiquidity);

      const poolTXCount: any = await getPoolTXCount(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      );
      console.log("poolTXCount", poolTXCount);
    };

    fetchFromEndPoint();

    return () => {};
  }, [dispatch]);

  return null;
}
