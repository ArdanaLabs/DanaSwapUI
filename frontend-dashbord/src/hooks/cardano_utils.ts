// import * as CardanoWasm from "@emurgo/cardano-serialization-lib-browser"

const cardano = (window as any).cardano;

export const connectWallet = async (): Promise<any> => {
  return await cardano.yoroi.enable()
};
