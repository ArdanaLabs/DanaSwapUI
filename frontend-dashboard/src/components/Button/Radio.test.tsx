import { Box, FormControl, FormLabel, RadioGroup } from "@material-ui/core"
import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { Radio } from "."

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
const option = {
  title: "Slippage",
  data: [
    { label: "0.5%", value: "0.5" },
    { label: "1%", value: "1" },
    { label: "", value: "Custom", hasInput: true },
  ],
}

describe("Components / Button / Radio", () => {
  beforeEach(() => {
    wrapper = shallow(<Radio option={option} value={option.data[0].value} />)
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should contain two 3 Box component", () => {
    expect(wrapper.find(Box)).toHaveLength(3)
  })
  it("should contain FormControl component", () => {
    expect(wrapper.find(FormControl)).toHaveLength(1)
  })
  it("should contain FormLabel component when title is not empty", () => {
    expect(wrapper.find(FormLabel)).toHaveLength(1)
  })
  it("should contain RadioGroup component", () => {
    expect(wrapper.find(RadioGroup)).toHaveLength(1)
  })
})
