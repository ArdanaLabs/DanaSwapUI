import { useEffect } from "react"
import { useDispatch } from "react-redux"
import { AppDispatch } from "state"
import {
  getAggVolume,
  getAggLiquidity,
  getPoolFees,
  getPoolVolume,
  getPoolLiquidity,
  getPoolTXCount,
  getPoolAPY,
  getPoolTransactions,
} from "./hooks"
import { FiveMinutes, OneDay, OneWeek } from "config/grains"
import { Any } from "config/txTypes"

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>()

  useEffect(() => {
    const fetchAggVolume = async () => {
      const aggVolume: any = await getAggVolume(
        "2020-12-12T00:00:00.0Z",
        "2020-12-12T00:05:00.0Z",
        FiveMinutes
      )
      console.log("aggVolume", aggVolume)
    }
    const fetchAggLiquidity = async () => {
      const aggLiquidity: any = await getAggLiquidity(
        "2020-12-12T00:00:00.0Z",
        "2020-12-14T00:00:00.0Z",
        OneDay
      )
      console.log("aggLiquidity", aggLiquidity)
    }
    const fetchPoolFees = async () => {
      const poolFees: any = await getPoolFees(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      )
      console.log("poolFees", poolFees)
    }
    const fetchPoolVolume = async () => {
      const poolVolume: any = await getPoolVolume(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      )
      console.log("poolVolume", poolVolume)
    }
    const fetchPoolLiquidity = async () => {
      const poolLiquidity: any = await getPoolLiquidity(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      )
      console.log("poolLiquidity", poolLiquidity)
    }
    const fetchPoolTxCount = async () => {
      const poolTXCount: any = await getPoolTXCount(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      )
      console.log("poolTXCount", poolTXCount)
    }
    const fetchPoolAPY = async () => {
      const poolAPY: any = await getPoolAPY(
        "foo",
        "2020-12-12T00:00:00.0Z",
        "2021-01-12T00:00:00.0Z",
        OneWeek
      )
      console.log("poolAPY", poolAPY)
    }
    const fetchPoolTransactions = async () => {
      const poolTransactions: any = await getPoolTransactions("foo", 0, 10, Any)
      console.log("poolTransactions", poolTransactions)
    }

    fetchAggVolume()
    fetchAggLiquidity()
    fetchPoolFees()
    fetchPoolVolume()
    fetchPoolLiquidity()
    fetchPoolTxCount()
    fetchPoolAPY()
    fetchPoolTransactions()
    return () => {}
  }, [dispatch])

  return null
}
