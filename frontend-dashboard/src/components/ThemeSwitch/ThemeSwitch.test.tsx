import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import ThemeSwitch from "."

Enzyme.configure({ adapter: new Adapter() })

const mockUserState: UserState = {
  theme: O.none,
  prefersColorScheme: O.none,
  timestamp: 0,
}

jest.mock("react-redux", () => ({
  useDispatch: () => {},
  useSelector: (): UserState => mockUserState,
}))

let wrapper: Enzyme.ShallowWrapper

describe("Components / ThemeSwitch / ThemeSwitch", () => {
  beforeEach(() => {
    wrapper = shallow(<ThemeSwitch />)
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })

  it("should contain one image", () => {
    expect(wrapper.find("img")).toHaveLength(1)
  })
})
