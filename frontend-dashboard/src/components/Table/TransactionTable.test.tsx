import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { BrowserRouter } from "react-router-dom"
import { TransactionTable } from "."

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

describe("Components / Table / TransactionTable", () => {
  beforeEach(() => {
    wrapper = shallow(
      <BrowserRouter basename="/">
        <TransactionTable />
      </BrowserRouter>
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
})
