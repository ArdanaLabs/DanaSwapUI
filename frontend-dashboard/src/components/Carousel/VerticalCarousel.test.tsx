import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import VerticalCarousel from "."

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

describe("Components / Carousel / VerticalCarousel", () => {
  beforeEach(() => {
    wrapper = shallow(
      <VerticalCarousel
        activeIndex={0}
        setActiveIndex={() => {}}
        data={["DANASWAP", `ARDANA VAULTS`, "MY DASHBOARD"]}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should contain 3 button", () => {
    expect(wrapper.find("button")).toHaveLength(3)
  })
  it("should contain correct button labels", () => {
    expect(wrapper.find("button").at(0).text()).toEqual("DANASWAP")
    expect(wrapper.find("button").at(1).text()).toEqual("ARDANA VAULTS")
    expect(wrapper.find("button").at(2).text()).toEqual("MY DASHBOARD")
  })
})
