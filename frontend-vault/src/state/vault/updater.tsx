import { useEffect } from "react"
import { useVault } from "./hooks"

const interval = 1000 * 60 // 1 min

export default function Updater() {
  const { updateVaults } = useVault()

  useEffect(() => {
    updateVaults()
    const timer = setInterval(() => {
      updateVaults()
    }, interval)

    return () => {
      clearInterval(timer)
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return null
}
