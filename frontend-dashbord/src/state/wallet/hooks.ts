import Axios from "axios";
import { useDispatch, useSelector } from "react-redux";

import { AppDispatch, AppState } from "state";
import { BalanceType, getAddressAction, getBalancesAction } from "./actions";

export function useWallet(): any {
  const dispatch = useDispatch<AppDispatch>();
  const wallet = useSelector<AppState, AppState["wallet"]>(
    (state) => state.wallet
  );

  const getBalances = async (address: string) => {
    if (!address) {
      return;
    }
    const balances = await fetchBalances(address);
    dispatch(getBalancesAction(balances));
  };

  const getAddress = async () => {
    dispatch(getAddressAction(''));
  };

  return {
    wallet,
    getBalances,
    getAddress,
  };
}

const fetchBalances = async (address: string): Promise<BalanceType[]> => {
  return Axios.get(`${process.env.REACT_APP_REST_URL}/addresses/${address}`, {
    headers: {
      project_id: process.env.REACT_APP_API_KEY as string,
    },
  })
    .then((response) => response.data)
    .then((response: any) => response.amount)
    .catch(() => []);
};
