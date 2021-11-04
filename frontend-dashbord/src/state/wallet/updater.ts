// import { checkWalletExtension, isWalletEnabled } from "hooks/cardano_utils";
import { useEffect } from "react";
import { useWallet } from "./hooks";

export default function Updater(): null {
  const { getBalances } = useWallet()
  
  useEffect(() => {
    // test
    (async () => {
      // if (checkWalletExtension()) {
      //   console.log(12222)
      //   console.log(1111, await isWalletEnabled())
      // }
      getBalances("addr_test1qz0neuwrkun2x6y6u5revx3pdsqm95gma74gxnt6scwrfpg5qujnwm0ncqv66ne9fl68sqk0nnkhzaga99phk052r4xsh5qdnq");
    })();
    // eslint-disable-next-line
  }, []);

  return null;
}
