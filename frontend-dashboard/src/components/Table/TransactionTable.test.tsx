import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { BrowserRouter } from "react-router-dom"

import * as O from "fp-ts/Option"

import * as PoolSetName from "Data/Pool/PoolSetName"

import { UserState } from "state/user/reducer"
import { TransactionTable } from "."

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

describe("Components / Table / TransactionTable", () => {
  beforeEach(() => {
    wrapper = shallow(
      <BrowserRouter basename="/">
        <TransactionTable poolSet={PoolSetName.iso.wrap("foo")} />
      </BrowserRouter>
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
})
