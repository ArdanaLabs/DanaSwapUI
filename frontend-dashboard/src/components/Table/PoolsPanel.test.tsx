import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { PoolsPanel } from "."
//import { TableContainer } from "@material-ui/core"
//import { SearchInput } from "components/Input"

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

describe("Components / Table / PoolsPanel", () => {
  beforeEach(() => {
    wrapper = shallow(<PoolsPanel overView={true} />)
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  /*
  it("should render TableContainer", () => {
    expect(wrapper.find(TableContainer)).toHaveLength(1)
  })
  it("should not render filter area", () => {
    expect(wrapper.find(SearchInput)).toHaveLength(0)
  })
  it("should render filter area when overview prop is false", () => {
    wrapper = shallow(<PoolsPanel overView={false} />)
    expect(wrapper.find(SearchInput)).toHaveLength(1)
  })
  */
})
