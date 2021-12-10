import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import React from "react"
import { SearchInput } from "."

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
const onFilterChange = jest.fn()

describe("Components / Box / TokenBox", () => {
  beforeEach(() => {
    wrapper = shallow(
      <SearchInput
        className={"someClassName"}
        value={"someValue"}
        placeholder="SEARCH..."
        isIcon={true}
        onChange={onFilterChange}
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
    expect(wrapper.find("input").prop("type")).toEqual("text")
    expect(wrapper.find("input").prop("value")).toEqual("someValue")
    expect(wrapper.find("input").prop("className")).toEqual("someClassName")
    expect(wrapper.find("input").prop("onChange")).toEqual(onFilterChange)
    expect(wrapper.find("input").prop("placeholder")).toEqual("SEARCH...")
  })
  it("changes input value onChange event", () => {
    wrapper.find("input").simulate("change", "My new value")
    expect(onFilterChange).toBeCalledWith("My new value")
  })
})
