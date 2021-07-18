import { useSelector } from "react-redux";

import { AppState } from "state";
import { TotalState } from "./actions";

export function useTotalStates(): TotalState {
  const state: TotalState = useSelector<AppState, AppState["home"]>(
    (state) => state.home
  );

  return state;
}
