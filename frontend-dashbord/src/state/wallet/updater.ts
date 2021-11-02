import { checkWalletExtension, isWalletEnabled } from "hooks/cardano_utils";
import { useEffect } from "react";

export default function Updater(): null {
  useEffect(() => {
    // test
    (async () => {
      // if (checkWalletExtension()) {
      //   console.log(12222)
      //   console.log(1111, await isWalletEnabled())
      // }
    })();
  }, []);

  return null;
}
