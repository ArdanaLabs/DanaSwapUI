import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { SwapButton } from "."

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
