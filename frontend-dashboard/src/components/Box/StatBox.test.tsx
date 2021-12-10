import { Box } from "@material-ui/core"
import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { StatBox } from "."
import IMG_TVL from "assets/icons/tvl.png"

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

describe("Components / Box / StatBox", () => {
  beforeEach(() => {
    wrapper = shallow(
      <StatBox
        image={IMG_TVL}
        title="TOTAL VALUE LOCKED"
        content="$1,234,567"
        delay={0}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should contain two 6 Box component", () => {
    expect(wrapper.find(Box)).toHaveLength(6)
  })
  it("should contain one image tag", () => {
    expect(wrapper.find("img")).toHaveLength(1)
  })
  it("should contain correct title string", () => {
    expect(wrapper.find(Box).at(4).text()).toEqual("TOTAL VALUE LOCKED")
  })
  it("should contain correct content string", () => {
    expect(wrapper.find(Box).at(2).text()).toEqual("$1,234,567")
  })
})
