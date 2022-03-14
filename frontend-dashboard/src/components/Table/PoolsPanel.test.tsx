import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { TableContainer } from "@material-ui/core"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { SearchInput } from "components/Input"
import { PoolsPanel } from "."

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

describe("Components / Table / PoolsPanel", () => {
  beforeEach(() => {
    wrapper = shallow(<PoolsPanel overview={true} />)
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  /* this is loading, need to await the request
  it("should render TableContainer", () => {
    expect(wrapper.find(TableContainer)).toHaveLength(1)
  })
  */
  it("should not render filter area", () => {
    expect(wrapper.find(SearchInput)).toHaveLength(0)
  })
  it("should render filter area when overview prop is false", () => {
    wrapper = shallow(<PoolsPanel overview={false} />)
    expect(wrapper.find(SearchInput)).toHaveLength(1)
  })
})
