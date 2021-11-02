// import * as CardanoWasm from "@emurgo/cardano-serialization-lib-browser"

const cardano = (window as any).cardano;

export const checkWalletExtension = () => {
  return (cardano) && (cardano.yoroi);
};

export const isWalletEnabled = async () => {
  if (!checkWalletExtension()) {
    alert("Install Yoroi wallet!");
    return;
  }
  return await cardano.yoroi.isEnabled()
}

export const connectWallet = async (): Promise<any> => {
  if (!checkWalletExtension()) {
    alert("Install Yoroi wallet!");
    return;
  }
  return await cardano.yoroi.enable();
};
