import { useSelector } from "react-redux"
import configureMockStore from "redux-mock-store"
import { getStats, usePoolStats, useTotalStats } from "./hooks"

const mockStore = configureMockStore([])

let store: any
const initialState = {
  home: {
    totalDepositsAllPoolsUSD: null,
    totalDailyVolumeUSD: null,
    totalDailyFeeVolumeUSD: null,
    totalLiquidityUtilization: null,
    poolStats: null,
  },
}

const mockDispatch = jest.fn()
jest.mock("react-redux", () => ({
  ...jest.requireActual("react-redux"),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch,
}))

beforeEach(() => {
  store = mockStore(initialState)
  expect.extend({
    nullOrAny(received, expected) {
      if (received === null) {
        return {
          pass: true,
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      if (expected === String) {
        return {
          pass: typeof received == "string" || received instanceof String,
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      if (expected === Number) {
        return {
          pass: typeof received == "number" || received instanceof Number,
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      if (expected === Function) {
        return {
          pass: typeof received == "function" || received instanceof Function,
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      if (expected === Object) {
        return {
          pass: received !== null && typeof received == "object",
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      if (expected === Boolean) {
        return {
          pass: typeof received == "boolean",
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }

      /* jshint -W122 */
      /* global Symbol */
      if (typeof Symbol != "undefined" && this.expectedObject == Symbol) {
        return {
          pass: typeof received == "symbol",
          message: () =>
            `expected null or instance of ${this.utils.printExpected(
              expected
            )}, but received ${this.utils.printReceived(received)}`,
        }
      }
      /* jshint +W122 */

      return {
        pass: received instanceof expected,
        message: () =>
          `expected null or instance of ${this.utils.printExpected(
            expected
          )}, but received ${this.utils.printReceived(received)}`,
      }
    },
  })
})

describe("Home hooks", () => {
  beforeEach(() => {
    ;(useSelector as jest.Mock).mockImplementation((callback) => {
      return callback(initialState)
    })
  })

  afterEach(() => {
    ;(useSelector as jest.Mock).mockClear()
  })

  it("should fetch stats data from endpoint", async () => {
    const result = await getStats()
    if (!result) {
      return
    }

    expect(result).toMatchObject({
      totalDailyTxCount: expect.nullOrAny(Number),
      totalDailyFeeVolumeUSD: expect.nullOrAny(Number),
      totalDailyVolumeUSD: expect.nullOrAny(Object),
      totalDepositsAllPoolsUSD: expect.nullOrAny(Number),
      totalLiquidityUtilization: expect.nullOrAny(Number),
      poolStats: expect.nullOrAny(Object),
    })
  })

  it("should return stored total stats", () => {
    const {
      totalDepositsAllPoolsUSD,
      totalDailyVolumeUSD,
      totalDailyFeeVolumeUSD,
      totalLiquidityUtilization,
    } = useTotalStats()

    expect(totalDepositsAllPoolsUSD).toEqual(
      store.getState().home.totalDepositsAllPoolsUSD
    )
    expect(totalDailyVolumeUSD).toEqual(
      store.getState().home.totalDailyVolumeUSD
    )
    expect(totalDailyFeeVolumeUSD).toEqual(
      store.getState().home.totalDailyFeeVolumeUSD
    )
    expect(totalLiquidityUtilization).toEqual(
      store.getState().home.totalLiquidityUtilization
    )
  })

  it("should return stored pool stats", () => {
    const poolStats = usePoolStats()

    expect(poolStats).toEqual(store.getState().home.poolStats)
  })
})
