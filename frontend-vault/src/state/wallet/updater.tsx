import BigNumber from "bignumber.js"
import { useEffect } from "react"
import { useWallet } from "./hooks"

const interval = 1000 * 60 //  1 min

export default function Updater() {
  const {
    cardanoApi,
    address,
    updateMyVaults,
    updateBalance,
    // updateWalletAddress,
  } = useWallet()

  useEffect(() => {
    if (!cardanoApi) {
      return () => {}
    }

    cardanoApi.getBalance().then((balance: string) => {
      updateBalance(new BigNumber(balance))
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
