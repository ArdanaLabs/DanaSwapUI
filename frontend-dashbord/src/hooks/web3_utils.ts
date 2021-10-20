
const cardano = (window as any).cardano;

export const isEnabled = async () => {
  return (await cardano) && cardano.isEnabled();
};

export const connectWallet = async () => {
  const enabled = await isEnabled();
  if (!enabled) {
    await cardano.enable();
  }
  // console.log(cardano)
  // console.log(await getBalance())

  // console.log(CardanoWeb3);
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
