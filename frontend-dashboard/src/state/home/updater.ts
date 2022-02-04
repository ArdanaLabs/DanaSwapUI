import { useEffect } from "react"
import { useDispatch } from "react-redux"

import * as Task from "fp-ts/Task"
import * as IO from "fp-ts/IO"

import { FetchDecodeResult } from "Data/FetchDecode"
import { CombinedStats } from "Data/Stats/CombinedStats"

import { AppDispatch } from "state"
import { receivedCombinedStats } from "./actions"
import { fetchPoolCombinedStats } from "./hooks"

// Unsure about this, but `null` is returned to satisfy the JSX
export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>()

  useEffect(() => {
    const fetchCombinedStats = Task.chainFirstIOK(
      (_combined: FetchDecodeResult<CombinedStats>) => {
        // side-effect
        dispatch(receivedCombinedStats(_combined))
        return IO.of(void 0)
      }
    )(fetchPoolCombinedStats())

    let timer = setInterval(fetchPoolCombinedStats, 1000 * 60 * 100)

    fetchCombinedStats()

    return () => {
      clearInterval(timer)
    }
  }, [dispatch])

  return null
}
