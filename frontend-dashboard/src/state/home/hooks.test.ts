import "@relmify/jest-fp-ts"
import { useSelector } from "react-redux"
import configureMockStore from "redux-mock-store"

import * as E from "fp-ts/Either"

import { fetchPoolCombinedStats } from "./hooks"

const mockStore = configureMockStore([])

let store: any
const initialState = {
  home: require("./reducer").initialState,
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
    const result = await fetchPoolCombinedStats()()

    if (E.isLeft(result)) {
      console.log(JSON.stringify(result.left, null, 2))
    }

    expect(result).toBeRight()
  })
})
