import { useEffect } from "react"
import { useWallet } from "./hooks"

const interval = 1000 * 60 //  1 min

export default function Updater() {
  const { address, updateMyVaults, updateBalance } = useWallet()

  useEffect(() => {
    if (!address) {
      return () => {}
    }

    updateBalance()
    updateMyVaults()

    const timer = setInterval(() => {
      updateBalance()
      updateMyVaults()
    }, interval)

    return () => {
      clearInterval(timer)
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [address])

  return null
}
