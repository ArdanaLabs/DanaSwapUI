const cardano = (window as any).cardano;

export const isEnabled = async () => {
  return (await cardano) && cardano.isEnabled();
};

export const connectWallet = async () => {
  const enabled = await isEnabled();
  if (!enabled) {
    await cardano.enable();
  }

  console.log(cardano);
};
