import { useSelector } from "react-redux";

import { AppState } from "state";
import { TotalStat } from "./actions";

export function useTotalStats(): TotalStat {
  const state: TotalStat = useSelector<AppState, AppState["home"]>(
    (state) => state.home
  );

  return state;
}
