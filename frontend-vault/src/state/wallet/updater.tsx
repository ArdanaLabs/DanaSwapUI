import BigNumber from "bignumber.js"
import { useEffect } from "react"
import { useWallet } from "./hooks"

const interval = 1000 * 60 //  1 min

export default function Updater() {
  const CardanoWasm = (window as any).CardanoWasm
  const {
    cardanoApi,
    address,
    updateMyVaults,
    updateBalances,
    updateWalletAddress,
  } = useWallet()

  useEffect(() => {
    if (!cardanoApi) {
      return () => {}
    }

    cardanoApi.getBalance("ADA").then((balance: string) => {
      updateBalances({ ada: new BigNumber(balance) })
    })

    cardanoApi.getUsedAddresses().then(function (addresses: string[]) {
      const addr = CardanoWasm.Address.from_bytes(
        Buffer.from(addresses[0], "hex")
      )
      updateWalletAddress(
        addr.to_bech32(
          process.env.REACT_APP_NETWORK_TYPE === "testnet"
            ? "addr_test"
            : "addr"
        )
      )
    })
    // eslint-disable-next-line
  }, [cardanoApi])

  useEffect(() => {
    if (!address) {
      return () => {}
    }

    updateMyVaults()

    const timer = setInterval(() => {
      updateMyVaults()
    }, interval)

    return () => {
      clearInterval(timer)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [address])

  return null
}
