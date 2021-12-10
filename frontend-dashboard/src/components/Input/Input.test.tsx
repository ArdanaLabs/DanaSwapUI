import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { Input } from "."

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
const onCustomInputChange = jest.fn()

describe("Components / Input / Input", () => {
  beforeEach(() => {
    wrapper = shallow(
      <Input
        placeholder={"Custom Amount"}
        value={1}
        className={"someClassName"}
        onChange={onCustomInputChange}
        type={"number"}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should contain input tag", () => {
    expect(wrapper.find("input")).toBeTruthy()
  })
  it("should be correct props", () => {
    expect(wrapper.find("input").prop("placeholder")).toEqual("Custom Amount")
    expect(wrapper.find("input").prop("value")).toEqual(1)
    expect(wrapper.find("input").prop("className")).toEqual("someClassName")
    expect(wrapper.find("input").prop("onChange")).toEqual(onCustomInputChange)
    expect(wrapper.find("input").prop("type")).toEqual("number")
    expect(wrapper.find("input").prop("step")).toEqual("0.1")
  })
  it("changes input value onChange event", () => {
    wrapper.find("input").simulate("change", "My new value")
    expect(onCustomInputChange).toBeCalledWith("My new value")
  })
})
