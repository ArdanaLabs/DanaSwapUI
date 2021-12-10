import { updateMediaDarkMode, updateUserDarkMode } from "./actions"

describe("User actions", () => {
  describe("updateMediaDarkMode", () => {
    it("should create action with passed data", () => {
      const mock = { mediaDarkMode: true }
      expect(updateMediaDarkMode(mock)).toEqual({
        type: "user/updateMediaDarkMode",
        payload: mock,
      })
    })
  })

  describe("updateUserDarkMode", () => {
    it("should create action with passed data", () => {
      const mock = { userDarkMode: true }
      expect(updateUserDarkMode(mock)).toEqual({
        type: "user/updateUserDarkMode",
        payload: mock,
      })
    })
  })
})
