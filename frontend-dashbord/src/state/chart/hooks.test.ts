import jsc from "jsverify"
import { useSelector } from 'react-redux'
import configureMockStore from 'redux-mock-store'
import { FiveMinutes } from 'config/grains'
import { useAggLiquidity, useAggVolume } from './hooks'
import { updateAggLiquidity, updateAggVolume } from './actions'

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

    jsc.assert(jsc.forall(jsc.string, jsc.string, jsc.string, getAggVolume));
  })

  it('should return stored aggLiquidity and getAggLiquidity function', async () => {
    const { aggLiquidity, getAggLiquidity } = useAggLiquidity()

    expect(aggLiquidity).toEqual(store.getState().chart.aggLiquidity)
    const result = await getAggLiquidity(
      '2020-12-12T00:00:00.0Z',
      '2020-12-12T00:05:00.0Z',
      FiveMinutes
    )
    if (!result) {
      return
    }
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(updateAggLiquidity(result))

    jsc.assert(jsc.forall(jsc.string, jsc.string, jsc.string, getAggLiquidity));
  })
})
