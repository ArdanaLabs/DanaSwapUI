import { useSelector } from "react-redux"

import { fetchJSON } from "fp-fetch"
import * as TaskEither from "fp-ts/TaskEither"
import * as RemoteData from "fp-ts-remote-data"

import * as CombinedStats from "Data/Stats/CombinedStats"
import { FetchDecodeTask, FetchDecodeError } from "Data/FetchDecode"

import { AppState } from "state"
import { apiURL } from "hooks"

export function useTotalStats(): RemoteData.RemoteData<
  FetchDecodeError,
  CombinedStats.TotalStats
> {
  return RemoteData.map(CombinedStats.totalStatsFromCombined)(
    useCombinedStats()
  )
}

export function usePoolStats(): RemoteData.RemoteData<
  FetchDecodeError,
  CombinedStats.PoolStatsMap.Type
> {
  return RemoteData.map((r: CombinedStats.Type) => r.poolStats)(
    useCombinedStats()
  )
}

export function useCombinedStats(): RemoteData.RemoteData<
  FetchDecodeError,
  CombinedStats.Type
> {
  return useSelector<AppState, AppState["home"]>((state) => state.home)
}

export function fetchPoolCombinedStats(): FetchDecodeTask<CombinedStats.Type> {
  const url: URL = apiURL("/combined")

  return TaskEither.chainEitherKW(CombinedStats.codec.decode)(
    fetchJSON(url.toString())
  )
}
