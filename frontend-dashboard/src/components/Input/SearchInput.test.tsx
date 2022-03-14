import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import React from "react"

import * as O from "fp-ts/Option"

import { UserState } from "state/user/reducer"
import { SearchInput } from "."

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
