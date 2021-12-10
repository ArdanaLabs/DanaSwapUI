import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { SwapButton } from "."

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
const onSwapButtonClick = () => {}

describe("Components / Button / SwapButton", () => {
  beforeEach(() => {
    wrapper = shallow(
      <SwapButton
        style={{ margin: "0 40px" }}
        onButtonClick={onSwapButtonClick}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should be correct props", () => {
    expect(wrapper.prop("onClick")).toEqual(onSwapButtonClick)
    expect(wrapper.prop("style")).toEqual({ margin: "0 40px" })
  })
  it("should contain 2 image tag", () => {
    expect(wrapper.find("img")).toHaveLength(2)
  })
})
