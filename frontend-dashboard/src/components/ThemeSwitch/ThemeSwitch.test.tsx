import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import ThemeSwitch from "."

Enzyme.configure({ adapter: new Adapter() })

jest.mock("react-redux", () => ({
  useDispatch: () => {},
  useSelector: () => ({
    user: {
      userDarkMode: null,
      mediaDarkMode: false,
      timestamp: "dfd",
    },
  }),
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
