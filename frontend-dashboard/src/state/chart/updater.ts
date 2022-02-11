import { useEffect } from "react"
import { useDispatch } from "react-redux"

import {
  fetchAggVolume,
  fetchAggLiquidity,
  fetchPoolFees,
  fetchPoolVolume,
  fetchPoolLiquidity,
  fetchPoolTxCount,
  fetchPoolAPY,
  fetchPoolTransactions,
} from "./hooks"
import { Granularity } from "Data/Chart/Granularity"
import { TransactionType } from "Data/Chart/TransactionType"
import * as PoolSetName from "Data/Pool/PoolSetName"

import { AppDispatch } from "state"

export default function Updater(): null {
  const dispatch = useDispatch<AppDispatch>()

  useEffect(() => {
    const foo: PoolSetName.Type = PoolSetName.iso.wrap("foo")

    fetchAggVolume(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-12T00:05:00.0Z")],
      Granularity.FiveMinutes
    )()
    fetchAggLiquidity(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-14T00:00:00.0Z")],
      Granularity.OneDay
    )()
    fetchPoolFees(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolVolume(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolLiquidity(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolTxCount(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolAPY(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()
    fetchPoolTransactions(foo, 0 | 0, 10 | 0, TransactionType.Any)()

    return () => {}
  }, [dispatch])

  return null
}
