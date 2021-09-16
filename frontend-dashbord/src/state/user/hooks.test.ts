import { useSelector } from 'react-redux'
import configureMockStore from 'redux-mock-store'
import { updateUserDarkMode } from './actions'
import { useIsDarkMode, useDarkModeManager } from './hooks'

const mockStore = configureMockStore([])

let store: any
const initialState = {
  user: {
    userDarkMode: null,
    mediaDarkMode: false,
    timestamp: new Date().getTime()
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

describe('User hooks', () => {
  beforeEach(() => {
    ;(useSelector as jest.Mock).mockImplementation(callback => {
      return callback(initialState)
    })
  })

  afterEach(() => {
    ;(useSelector as jest.Mock).mockClear()
  })

  describe('useIsDarkMode', () => {
    it('should return darkMode status', () => {
      const darkMode = useIsDarkMode()
      const userDarkMode = store.getState().user.userDarkMode
      const mediaDarkMode = store.getState().user.mediaDarkMode

      expect(darkMode).toEqual(
        userDarkMode === null ? mediaDarkMode : userDarkMode
      )
    })
  })

  describe('useDarkModeManager', () => {
    it('should return darkMode status and setDarkMode function', () => {
      const expectedDarkMode = useIsDarkMode()
      const [darkMode, setDarkMode] = useDarkModeManager()

      expect(darkMode).toEqual(expectedDarkMode)

      setDarkMode(true)
      expect(mockDispatch).toHaveBeenCalledTimes(1)
      expect(mockDispatch).toHaveBeenCalledWith(
        updateUserDarkMode({ userDarkMode: true })
      )
    })
  })
})
