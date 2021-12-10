import reducer, { initialState } from "./reducer"
import { updateMediaDarkMode, updateUserDarkMode } from "./actions"

describe("User reducers", () => {
  it("should return mediaDarkMode value when dispatch updateMediaDarkMode action", () => {
    const mock = { mediaDarkMode: true }
    expect(
      reducer(initialState, updateMediaDarkMode(mock)).mediaDarkMode
    ).toEqual(true)
  })

  it("should return userDarkMode value when dispatch updateUserDarkMode action", () => {
    const mock = { userDarkMode: true }
    expect(
      reducer(initialState, updateUserDarkMode(mock)).userDarkMode
    ).toEqual(true)
  })
})
