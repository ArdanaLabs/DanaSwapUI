import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { Button } from "."

Enzyme.configure({ adapter: new Adapter() })

let wrapper: Enzyme.ShallowWrapper

describe("Components / Button / Button", () => {
  beforeEach(() => {
    wrapper = shallow(
      <Button onClick={() => {}} variant="contained" className="someClassName">
        Click Me
      </Button>
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should be correct props", () => {
    expect(wrapper.prop("className")).toEqual("someClassName")
    expect(wrapper.prop("variant")).toEqual("contained")
  })
  it("should include specific string", () => {
    expect(wrapper.text()).toEqual("Click Me")
  })
})
