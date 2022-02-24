import { Box } from "@material-ui/core"
import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { Dialog } from "components/Dialog"
import { TokenBox } from "."

import LOGO_Ardana from "assets/logos/ardana.png"

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

describe("Components / Box / TokenBox", () => {
  beforeEach(() => {
    wrapper = shallow(
      <TokenBox
        label="SEND"
        token={{
          src: LOGO_Ardana,
          name: "DANA",
          desc: "exDANA",
        }}
        amount={1}
        onMaxAmount={() => {}}
        handleTokenSelect={() => {}}
        className={"someClassName"}
        style={{ padding: 10 }}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should be correct props", () => {
    expect(wrapper.prop("className")).toEqual("someClassName")
    expect(wrapper.prop("style")).toEqual({ padding: 10 })
  })
  it("should contain someClassName", () => {
    expect(wrapper.find(".someClassName")).toHaveLength(1)
  })
  it("should contain two 22 Box component", () => {
    expect(wrapper.find(Box)).toHaveLength(22)
  })
  it("should contain 3 image tag", () => {
    expect(wrapper.find("img")).toHaveLength(3)
  })
  it("should contain correct label string", () => {
    expect(wrapper.text().includes("SEND")).toBeTruthy()
  })
  it("should contain Dialog component", () => {
    expect(wrapper.find(Dialog)).toHaveLength(1)
  })
  it("should have max amount button", () => {
    expect(wrapper.find("#max_button")).toHaveLength(1)
  })
})
