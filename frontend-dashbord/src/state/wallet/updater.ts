// import { checkWalletExtension, isWalletEnabled } from "hooks/cardano_utils";
import { useEffect } from "react";
import { useWallet } from "./hooks";

const NETWORK: string = process.env.REACT_APP_NETWORK || "mainnet";

export default function Updater(): null {
  const {
    wallet: { cardanoApi, address },
    getBalances,
    getAddress,
  } = useWallet();

  useEffect(() => {
    // test
    (async () => {})();
    // eslint-disable-next-line
  }, []);

  useEffect(() => {
    if (cardanoApi) {
      (async () => {
        const CardanoWasm = await import(
          "@emurgo/cardano-serialization-lib-browser"
        );
        const addr = CardanoWasm.Address.from_bytes(
          Buffer.from(
            "009f3cf1c3b726a3689ae507961a216c01b2d11befaa834d7a861c3485140725376df3c019ad4f254ff47802cf9ced71751d29437b3e8a1d4d",
            "hex"
          )
        );
        getAddress(
          addr.to_bech32(NETWORK !== "mainnet" ? "addr" : "addr_test")
        );
      })();
    }
    // eslint-disable-next-line
  }, [cardanoApi]);

  useEffect(() => {
    if (address) {
      getBalances(address);
    }
    // eslint-disable-next-line
  }, [address]);

  return null;
}
