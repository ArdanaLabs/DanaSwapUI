import Cardano from "services/cardano";
import { useDispatch, useSelector } from "react-redux";

import { AppDispatch, AppState } from "state";
import { getAccountKeys as _getAccountKeys } from "./actions";

export function useWallet(): any {
  const dispatch = useDispatch<AppDispatch>();
  const wallet = useSelector<AppState, AppState["wallet"]>(
    (state) => state.wallet
  );

  const getAccountKeys = async (mnemonic: string) => {
    const accountInfo: any = await Cardano.crypto.getAccountKeys(mnemonic);
    accountInfo && dispatch(_getAccountKeys(accountInfo));
    return accountInfo;
  };

  return {
    wallet,
    getAccountKeys,
  };
}
