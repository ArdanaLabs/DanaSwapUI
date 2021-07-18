import { useEffect } from "react";
import { useDispatch } from "react-redux";

import { AppDispatch } from "state";
import { updateTotalStates } from "./actions";

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>();

  useEffect(() => {
    dispatch(
      updateTotalStates({
        totalDepositsAllPoolsUSD: 20,
        totalDailyVolumeUSD: 30,
      })
    );
    return () => {};
  }, [dispatch]);

  return null;
}
