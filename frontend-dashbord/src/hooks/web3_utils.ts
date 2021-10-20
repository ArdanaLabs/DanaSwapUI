import Cardano from "services/cardano";

const cardano = (window as any).cardano;

export const isEnabled = async () => {
  return (await cardano) && cardano.isEnabled();
};

export const connectWallet = async () => {
  const enabled = await isEnabled();
  if (!enabled) {
    await cardano.enable();
  }

  const mnemonic =
    "advice leave liar scan palm win use know rent destroy cruel defense eager coil glory glimpse tower junior body orphan father minute series opera";
  const { privateKey, publicKey }: any = await Cardano.crypto.getAccountKeys(
    mnemonic
  );
  console.log(publicKey);
  console.log(privateKey);
  console.log(await Cardano.explorer.getNetworkInfo())
  const { assets, transactions, utxos } =
    await Cardano.explorer.getAccountStateByPublicKey(
      publicKey,
      25,
      10,
      [0, 1]
    ); // will fecth 10 times by 25 addresse internal and external ([0, 1]) addresses

  console.log("accountBalance", assets);
  console.log("accountTransactions", transactions);
  console.log("accountUtxos", utxos);

  console.log(await Cardano.crypto.getAccountAddresses(publicKey));
};

export const getNetworkId = async () => {
  return await cardano.getNetworkId();
  // 0 - testnet
  // 1 = mainnet
};

export const getUsedAddresses = async () => {
  return await cardano.getUsedAddresses();
};

export const getBalance = async () => {
  return await cardano.getBalance();
};
