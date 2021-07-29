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

      totalStats && dispatch(updateTotalStats(totalStats));
    };

    let timer = setInterval(async () => {
      await fetchTotalStatsFromEndPoint();
    }, 1000 * 60 * 100);

    fetchTotalStatsFromEndPoint();

    return () => {
      clearInterval(timer);
    };
  }, [dispatch]);

  return null;
}
