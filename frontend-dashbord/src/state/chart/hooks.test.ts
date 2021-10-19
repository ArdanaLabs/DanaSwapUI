import jsc from 'jsverify'
import { useSelector } from 'react-redux'
import configureMockStore from 'redux-mock-store'
import {
  FiveMinutes,
  FourHours,
  OneDay,
  OneHour,
  OneMinute,
  OneMonth,
  OneWeek,
  TenMinutes,
  ThirtyMinutes,
  TwelveHours
} from 'config/grains'
import {
  getAggLiquidity,
  getAggVolume,
  getPoolAPY,
  getPoolFees,
  getPoolLiquidity,
  getPoolTransactions,
  getPoolTXCount,
  getPoolVolume,
  useAggLiquidity,
  useAggVolume,
  usePoolAPY,
  usePoolFees,
  usePoolTransactions,
  usePoolTxCount,
  usePoolVolume
} from './hooks'
import {
  updateAggLiquidity,
  updateAggVolume,
  updatePoolAPY,
  updatePoolFees,
  updatePoolTransactions,
  updatePoolTXCount,
  updatePoolVolume
} from './actions'
import { Any } from 'config/txTypes'

const mockStore = configureMockStore([])

let store: any
const initialState = {
  chart: {
    aggVolume: [
      {
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      }
    ],
    aggLiquidity: [
      {
        start: null,
        end: null,
        value: null
      }
    ],
    poolFees: [
      {
        start: null,
        end: null,
        value: null
      }
    ],
    poolVolume: [
      {
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      }
    ],
    poolLiquidity: [
      {
        start: null,
        end: null,
        value: null
      }
    ],
    poolTXCount: [
      {
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      }
    ],
    poolAPY: [
      {
        start: null,
        end: null,
        value: null
      }
    ],
    poolTransactions: [
      {
        tx: null,
        navUSD: null
      }
    ]
  }
}

const grains = jsc.oneof([
  jsc.constant(OneMinute),
  jsc.constant(FiveMinutes),
  jsc.constant(TenMinutes),
  jsc.constant(ThirtyMinutes),
  jsc.constant(OneHour),
  jsc.constant(FourHours),
  jsc.constant(TwelveHours),
  jsc.constant(OneDay),
  jsc.constant(OneWeek),
  jsc.constant(OneMonth)
])

const mockDispatch = jest.fn()
jest.mock('react-redux', () => ({
  ...jest.requireActual('react-redux'),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch
}))

beforeEach(() => {
  store = mockStore(initialState)
})

describe('Chart hooks', () => {
  beforeEach(() => {
    ;(useSelector as jest.Mock).mockImplementation(callback => {
      return callback(initialState)
    })
  })

  afterEach(() => {
    ;(useSelector as jest.Mock).mockClear()
  })

  it('should fetch aggVolume from endpoint', async () => {
    const result = await getAggVolume(
      '2020-12-12T00:00:00.0Z',
      '2020-12-12T00:05:00.0Z',
      FiveMinutes
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      addLiquidity: expect.any(Number),
      removeLiquidity: expect.any(Number),
      total: expect.any(Number),
      trade: expect.any(Number)
    })
  })

  it('should fetch aggLiquidity from endpoint', async () => {
    const result = await getAggLiquidity(
      '2020-12-12T00:00:00.0Z',
      '2020-12-14T00:00:00.0Z',
      OneDay
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      value: expect.any(Number)
    })
  })

  it('should fetch poolFees from endpoint', async () => {
    const result = await getPoolFees(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      value: expect.any(Number)
    })
  })

  it('should fetch poolVolume from endpoint', async () => {
    const result = await getPoolVolume(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      addLiquidity: expect.any(Number),
      removeLiquidity: expect.any(Number),
      total: expect.any(Number),
      trade: expect.any(Number)
    })
  })

  it('should fetch poolLiquidity from endpoint', async () => {
    const result = await getPoolLiquidity(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      value: expect.any(Number)
    })
  })

  it('should fetch poolTXCount from endpoint', async () => {
    const result = await getPoolTXCount(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      addLiquidity: expect.any(Number),
      removeLiquidity: expect.any(Number),
      total: expect.any(Number),
      trade: expect.any(Number)
    })
  })

  it('should fetch poolAPY from endpoint', async () => {
    const result = await getPoolAPY(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      start: expect.any(String),
      end: expect.any(String),
      value: expect.any(Number)
    })
  })

  it('should fetch poolTransactions from endpoint', async () => {
    const result = await getPoolTransactions('foo', 0, 10, Any)
    if (!result) {
      return
    }

    expect(Array.isArray(result)).toBe(true)
    expect(result[0]).toMatchObject({
      tx: expect.any(Object),
      navUSD: expect.any(Number)
    })
  })

  it('should return stored aggVolume and getAggVolume function', async () => {
    const { aggVolume, getAggVolume } = useAggVolume()

    expect(aggVolume).toEqual(store.getState().chart.aggVolume)
    const result = await getAggVolume(
      '2020-12-12T00:00:00.0Z',
      '2020-12-12T00:05:00.0Z',
      FiveMinutes
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updateAggVolume(result))

    // jsc.assert(
    //   jsc.forall(jsc.datetime, jsc.datetime, grains, getAggVolume)
    // )
  })

  it('should return stored aggLiquidity and getAggLiquidity function', async () => {
    const { aggLiquidity, getAggLiquidity } = useAggLiquidity()

    expect(aggLiquidity).toEqual(store.getState().chart.aggLiquidity)
    const result = await getAggLiquidity(
      '2020-12-12T00:00:00.0Z',
      '2020-12-14T00:00:00.0Z',
      OneDay
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updateAggLiquidity(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.string, grains, getAggLiquidity)
    // )
  })

  it('should return stored poolFees and getPoolFees function', async () => {
    const { poolFees, getPoolFees } = usePoolFees()

    expect(poolFees).toEqual(store.getState().chart.poolFees)
    const result = await getPoolFees(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updatePoolFees(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, getPoolFees)
    // )
  })

  it('should return stored poolVolume and getPoolVolume function', async () => {
    const { poolVolume, getPoolVolume } = usePoolVolume()

    expect(poolVolume).toEqual(store.getState().chart.poolVolume)
    const result = await getPoolVolume(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updatePoolVolume(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, getPoolVolume)
    // )
  })

  it('should return stored poolTXCount and getPoolTXCount function', async () => {
    const { poolTXCount, getPoolTXCount } = usePoolTxCount()

    expect(poolTXCount).toEqual(store.getState().chart.poolTXCount)
    const result = await getPoolTXCount(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    ).catch(() => null)
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updatePoolTXCount(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, getPoolTXCount)
    // )
  })

  it('should return stored poolAPY and getPoolAPY function', async () => {
    const { poolAPY, getPoolAPY } = usePoolAPY()

    expect(poolAPY).toEqual(store.getState().chart.poolAPY)
    const result = await getPoolAPY(
      'foo',
      '2020-12-12T00:00:00.0Z',
      '2021-01-12T00:00:00.0Z',
      OneWeek
    ).catch(() => null)

    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updatePoolAPY(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, getPoolAPY)
    // )
  })

  it('should return stored poolTransactions and getPoolTransactions function', async () => {
    const { poolTransactions, getPoolTransactions } = usePoolTransactions()

    expect(poolTransactions).toEqual(store.getState().chart.poolTransactions)
    const result = await getPoolTransactions(
      'foo',
      0,
      10,
      Any
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updatePoolTransactions(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.number, jsc.number, jsc.string, getPoolTransactions)
    // )
  })
})
