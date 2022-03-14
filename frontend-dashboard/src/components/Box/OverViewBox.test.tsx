import { Box } from "@material-ui/core"
import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { OverViewBox } from "."

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

describe("Components / Box / OverViewBox", () => {
  beforeEach(() => {
    wrapper = shallow(<OverViewBox label={"TVL\n\n"} content={"$220.21 M"} />)
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should contain two 7 Box component", () => {
    expect(wrapper.find(Box)).toHaveLength(7)
  })
  it("should contain one image tag", () => {
    expect(wrapper.find("img")).toHaveLength(1)
  })
  it("should contain correct label string", () => {
    expect(wrapper.text().includes("TVL")).toBeTruthy()
  })
  it("should contain correct content string", () => {
    expect(wrapper.text().includes("$220.21 M")).toBeTruthy()
  })
})
