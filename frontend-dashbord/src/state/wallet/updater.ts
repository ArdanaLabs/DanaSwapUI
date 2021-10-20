import { useEffect } from "react";
import { useWallet } from "./hooks";

export default function Updater(): null {
  const { getAccountKeys } = useWallet();

  useEffect(() => {
    // test
    getAccountKeys(
      "advice leave liar scan palm win use know rent destroy cruel defense eager coil glory glimpse tower junior body orphan father minute series opera"
    );
  }, [getAccountKeys]);

  return null;
}
