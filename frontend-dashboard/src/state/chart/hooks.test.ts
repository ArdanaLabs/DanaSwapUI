// import { itProp, fc } from "jest-fast-check"
import "@relmify/jest-fp-ts"
// import { testProp, fc } from "jest-fast-check"
import { useSelector } from "react-redux"
import configureMockStore from "redux-mock-store"

import * as PoolSetName from "Data/Pool/PoolSetName"
import { Granularity } from "Data/Chart/Granularity"
import { TransactionType } from "Data/Chart/TransactionType"

import {
  fetchAggLiquidity,
  fetchAggVolume,
  fetchPoolAPY,
  fetchPoolFees,
  fetchPoolLiquidity,
  fetchPoolTransactions,
  fetchPoolTxCount,
  fetchPoolVolume,
  useAggLiquidity,
  useAggVolume,
  usePoolAPY,
  usePoolFees,
  usePoolTransactions,
  usePoolTxCount,
  usePoolVolume,
} from "./hooks"
import {
  receivedAggLiquidity,
  receivedAggVolume,
  receivedPoolAPY,
  receivedPoolFees,
  receivedPoolTransactions,
  receivedPoolTxCount,
  receivedPoolVolume,
} from "./actions"

const foo: PoolSetName.Type = PoolSetName.iso.wrap("foo")

const mockStore = configureMockStore([])

let store: any
const initialState = {
  chart: require("./reducer").initialState,
}

const mockDispatch = jest.fn()
jest.mock("react-redux", () => ({
  ...jest.requireActual("react-redux"),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch,
}))

beforeEach(() => {
  store = mockStore(initialState)
})

describe("Chart hooks", () => {
  beforeEach(() => {
    ;(useSelector as jest.Mock).mockImplementation((callback) => {
      return callback(initialState)
    })
  })

  afterEach(() => {
    ;(useSelector as jest.Mock).mockClear()
  })

  it("should fetch aggVolume from endpoint", async () => {
    const result = await fetchAggVolume(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-12T00:05:00.0Z")],
      Granularity.FiveMinutes
    )()

    expect(result).toBeRight()
  })

  it("should fetch aggLiquidity from endpoint", async () => {
    const result = await fetchAggLiquidity(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-14T00:00:00.0Z")],
      Granularity.OneDay
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolFees from endpoint", async () => {
    const result = await fetchPoolFees(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolVolume from endpoint", async () => {
    const result = await fetchPoolVolume(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolLiquidity from endpoint", async () => {
    const result = await fetchPoolLiquidity(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolTxCount from endpoint", async () => {
    const result = await fetchPoolTxCount(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolAPY from endpoint", async () => {
    const result = await fetchPoolAPY(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
  })

  it("should fetch poolTransactions from endpoint", async () => {
    const result = await fetchPoolTransactions(
      foo,
      0 | 0,
      10 | 0,
      TransactionType.Any
    )()

    expect(result).toBeRight()
  })

  it("should return stored aggVolume and fetchAggVolume function", async () => {
    const { aggVolume, fetchAggVolume } = useAggVolume()

    expect(aggVolume).toEqual(store.getState().chart.aggVolume)
    const result = await fetchAggVolume(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-12T00:05:00.0Z")],
      Granularity.FiveMinutes
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedAggVolume(result))
  })

  /*
  testProp(
    "fetchAggVolume properties",
    [genTimeInterval, genGranularity],
    async (timeInterval: TimeInterval, granularity: Granularity) => {
      const result = await fetchAggVolume(timeInterval, granularity)()
      expect(result).toBeRight()
    }
  )
  */

  it("should return stored aggLiquidity and fetchAggLiquidity function", async () => {
    const { aggLiquidity, fetchAggLiquidity } = useAggLiquidity()

    expect(aggLiquidity).toEqual(store.getState().chart.aggLiquidity)

    const result = await fetchAggLiquidity(
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2020-12-14T00:00:00.0Z")],
      Granularity.OneDay
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedAggLiquidity(result))
  })

  /*
  testProp(
    "fetchAggLiquidity properties",
    [genTimeInterval, genGranularity],
    async (timeInterval: TimeInterval, granularity: Granularity) => {
      const result = await fetchAggLiquidity(timeInterval, granularity)()
      expect(result).toBeRight()
    }
  )
  */

  it("should return stored poolFees and fetchPoolFees function", async () => {
    const { poolFees, fetchPoolFees } = usePoolFees()

    expect(poolFees).toEqual(store.getState().chart.poolFees)
    const result = await fetchPoolFees(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedPoolFees(result))
  })

  /*
  testProp(
    "fetchPoolFees properties with foo",
    [fc.constant(foo), genTimeInterval, genGranularity],
    async (
      poolSetName: PoolSetName,
      timeInterval: TimeInterval,
      granularity: Granularity
    ) => {
      const result = await fetchPoolFees(
        poolSetName,
        timeInterval,
        granularity
      )()
      E.isLeft(result) &&
        console.error(
          "Fetch decoding failure:",
          JSON.stringify(result.left, null, 2)
        )
      expect(result).toBeRight()
    }
  )
  */

  it("should return stored poolVolume and fetchPoolVolume function", async () => {
    const { poolVolume, fetchPoolVolume } = usePoolVolume()

    expect(poolVolume).toEqual(store.getState().chart.poolVolume)
    const result = await fetchPoolVolume(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedPoolVolume(result))
  })

  /*
  testProp(
    "fetchPoolVolume properties with foo",
    [fc.constant(foo), genTimeInterval, genGranularity],
    async (
      poolSetName: PoolSetName,
      timeInterval: TimeInterval,
      granularity: Granularity
    ) => {
      const result = await fetchPoolVolume(
        poolSetName,
        timeInterval,
        granularity
      )()
      expect(result).toBeRight()
    }
  )
  */

  it("should return stored poolTxCount and fetchPoolTxCount function", async () => {
    const { poolTxCount, fetchPoolTxCount } = usePoolTxCount()

    expect(poolTxCount).toEqual(store.getState().chart.poolTxCount)

    const result = await fetchPoolTxCount(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedPoolTxCount(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, fetchPoolTxCount)
    // )
  })

  it("should return stored poolAPY and fetchPoolAPY function", async () => {
    const { poolAPY, fetchPoolAPY } = usePoolAPY()

    expect(poolAPY).toEqual(store.getState().chart.poolAPY)
    const result = await fetchPoolAPY(
      foo,
      [new Date("2020-12-12T00:00:00.0Z"), new Date("2021-01-12T00:00:00.0Z")],
      Granularity.OneWeek
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedPoolAPY(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.datetime, jsc.datetime, jsc.string, fetchPoolAPY)
    // )
  })

  it("should return stored poolTransactions and fetchPoolTransactions function", async () => {
    const { poolTransactions, fetchPoolTransactions } = usePoolTransactions()

    expect(poolTransactions).toEqual(store.getState().chart.poolTransactions)
    const result = await fetchPoolTransactions(
      foo,
      0 | 0,
      10 | 0,
      TransactionType.Any
    )()

    expect(result).toBeRight()
    expect(mockDispatch).toHaveBeenCalledTimes(1)
    expect(mockDispatch).toHaveBeenCalledWith(receivedPoolTransactions(result))

    // jsc.assert(
    //   jsc.forall(jsc.string, jsc.number, jsc.number, jsc.string, fetchPoolTransactions)
    // )
  })
})
