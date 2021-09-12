import { useDispatch, useSelector } from 'react-redux'
import { AppDispatch, AppState } from 'state'
import { API_URL } from 'config/endpoints'
import {
  RangedVolume,
  updateAggLiquidity,
  updateAggVolume,
  updatePoolAPY,
  updatePoolFees,
  updatePoolLiquidity,
  updatePoolTransactions,
  updatePoolTXCount,
  updatePoolVolume
} from './actions'

export function useAggVolume () {
  const dispatch = useDispatch<AppDispatch>()
  const { aggVolume } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getAggVolume = async (start: string, end: string, grain: string) => {
    const _aggVolume = await getAggVolume(start, end, grain)
    _aggVolume && dispatch(updateAggVolume(_aggVolume))
    return _aggVolume
  }

  return {
    aggVolume,
    getAggVolume: _getAggVolume
  }
}
export function useAggLiquidity () {
  const dispatch = useDispatch<AppDispatch>()
  const { aggLiquidity } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getAggLiquidity = async (
    start: string,
    end: string,
    grain: string
  ) => {
    const _aggLiquidity = await getAggLiquidity(start, end, grain)
    _aggLiquidity && dispatch(updateAggLiquidity(_aggLiquidity))
    return _aggLiquidity
  }

  return {
    aggLiquidity,
    getAggLiquidity: _getAggLiquidity
  }
}
export function usePoolFees () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolFees } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )
  const _getPoolFees = async (
    poolName: string,
    start: string,
    end: string,
    grain: string
  ) => {
    const _poolFees = await getPoolFees(poolName, start, end, grain)
    _poolFees && dispatch(updatePoolFees(_poolFees))
    return _poolFees;
  }
  return {
    poolFees,
    getPoolFees: _getPoolFees
  }
}
export function usePoolVolume () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolVolume } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getPoolVolume = async (
    poolName: string,
    start: string,
    end: string,
    grain: string
  ) => {
    const _poolVolume = await getPoolVolume(poolName, start, end, grain)
    _poolVolume && dispatch(updatePoolVolume(_poolVolume))
    return _poolVolume
  }

  return {
    poolVolume,
    getPoolVolume: _getPoolVolume
  }
}
export function usePoolLiquidity () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolLiquidity } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getPoolLiquidity = async (
    poolName: string,
    start: string,
    end: string,
    grain: string
  ) => {
    const _poolLiquidity = await getPoolLiquidity(poolName, start, end, grain)
    _poolLiquidity && dispatch(updatePoolLiquidity(_poolLiquidity))
  }

  return {
    poolLiquidity,
    getPoolLiquidity: _getPoolLiquidity
  }
}
export function usePoolTxCount () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolTXCount } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getPoolTXCount = async (
    poolName: string,
    start: string,
    end: string,
    grain: string
  ) => {
    const _poolTXCount = await getPoolTXCount(poolName, start, end, grain)
    _poolTXCount && dispatch(updatePoolTXCount(_poolTXCount))
    return _poolTXCount
  }

  return {
    poolTXCount,
    getPoolTXCount: _getPoolTXCount
  }
}
export function usePoolAPY () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolAPY } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getPoolAPY = async (
    pool: string,
    start: string,
    end: string,
    grain: string
  ) => {
    const _poolAPY = await getPoolAPY(pool, start, end, grain)
    _poolAPY && dispatch(updatePoolAPY(_poolAPY))
    return _poolAPY
  }

  return {
    poolAPY,
    getPoolAPY: _getPoolAPY
  }
}
export function usePoolTransactions () {
  const dispatch = useDispatch<AppDispatch>()
  const { poolTransactions } = useSelector<AppState, AppState['chart']>(
    state => state.chart
  )

  const _getPoolTransactions = async (
    pool: string,
    start: number,
    end: number,
    type: string
  ) => {
    const _poolTransactions = await getPoolTransactions(pool, start, end, type)
    _poolTransactions && dispatch(updatePoolTransactions(_poolTransactions))
    return _poolTransactions
  }

  return {
    poolTransactions,
    getPoolTransactions: _getPoolTransactions
  }
}

// fetch from api server
export async function getAggVolume (start: string, end: string, grain: string) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/aggregate/volume?start=${start}&end=${end}&grain="${grain}"`
    )
    const aggVolume: any[] = await result.json()

    return aggVolume.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      addLiquidity: item[1].addLiquidity,
      removeLiquidity: item[1].removeLiquidity,
      total: item[1].total,
      trade: item[1].trade
    }))
  } catch (e) {
    // console.log('getAggVolume: error fetching', e)
    return null
  }
}

export async function getAggLiquidity (
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/aggregate/liquidity?start=${start}&end=${end}&grain="${grain}"`
    )
    const aggLiquidity: any[] = await result.json()

    return aggLiquidity.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      value: item[1]
    }))
  } catch (e) {
    // console.log('getAggLiquidity: error fetching', e)
    return null
  }
}

export async function getPoolFees (
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/fees?pool="${pool}"&start=${start}&end=${end}&grain="${grain}"`
    )
    const poolFees = await result.json()
    return poolFees.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      value: item[1]
    }))
  } catch (e) {
    // console.log('getPoolFees: error fetching', e)
    return null
  }
}

export async function getPoolVolume (
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/volume?pool="${pool}"&start=${start}&end=${end}&grain="${grain}"`
    )
    const poolVolume: any[] = await result.json()

    return poolVolume.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      addLiquidity: item[1].addLiquidity,
      removeLiquidity: item[1].removeLiquidity,
      total: item[1].total,
      trade: item[1].trade
    }))
  } catch (e) {
    // console.log('getPoolVolume: error fetching', e)
    return null
  }
}

export async function getPoolLiquidity (
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/liquidity?pool="${pool}"&start=${start}&end=${end}&grain="${grain}"`
    )
    const poolLiquidity: any[] = await result.json()

    return poolLiquidity.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      value: item[1]
    }))
  } catch (e) {
    // console.log('getPoolLiquidity: error fetching', e)
    return null
  }
}

export async function getPoolTXCount (
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/tx-count?pool="${pool}"&start=${start}&end=${end}&grain="${grain}"`
    )
    const poolTXCount = await result.json()
    return poolTXCount.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      addLiquidity: item[1].addLiquidity,
      removeLiquidity: item[1].removeLiquidity,
      total: item[1].total,
      trade: item[1].trade
    }))
  } catch (e) {
    // console.log('getPoolTXCount: error fetching', e)
    return null
  }
}

export async function getPoolAPY (
  pool: string,
  start: string,
  end: string,
  grain: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/chart/pool/apy?pool="${pool}"&start=${start}&end=${end}&grain="${grain}"`
    )
    const poolAPY: any[] = await result.json()
    return poolAPY.map((item: any) => ({
      start: item[0][0],
      end: item[0][1],
      value: item[1]
    }))
  } catch (e) {
    // console.log('getPoolAPY: error fetching', e)
    return null
  }
}

export async function getPoolTransactions (
  pool: string,
  start: number,
  end: number,
  type: string
) {
  try {
    const result = await fetch(
      API_URL +
        `/transactions?pool="${pool}"&start=${start}&end=${end}&type="${type}"`
    )

    return result.json()
  } catch (e) {
    // console.log('getPoolTransactions: error fetching', e)
    return null
  }
}
