import Adapter from "@wojtekmaj/enzyme-adapter-react-17"
import * as Enzyme from "enzyme"
import { shallow } from "enzyme"
import { Dialog } from "."

Enzyme.configure({ adapter: new Adapter() })

let wrapper: Enzyme.ShallowWrapper
const handleDialogClose = () => {
  console.log("goodbye")
}

describe("Components / Dialog / Dialog", () => {
  beforeEach(() => {
    wrapper = shallow(
      <Dialog
        onClose={handleDialogClose}
        aria-labelledby="simple-dialog-title"
        open={true}
      />
    )
  })

  it("renders", () => {
    expect(wrapper).toMatchSnapshot()
  })
  it("should be correct props", () => {
    expect(wrapper.prop("open")).toEqual(true)
    expect(wrapper.prop("aria-labelledby")).toEqual("simple-dialog-title")
    expect(wrapper.prop("onClose")).toEqual(handleDialogClose)
  })
})
