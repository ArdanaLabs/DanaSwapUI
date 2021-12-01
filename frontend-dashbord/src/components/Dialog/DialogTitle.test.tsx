import { Typography, IconButton } from "@material-ui/core";
import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import { shallow } from "enzyme";
import { DialogTitle } from ".";

Enzyme.configure({ adapter: new Adapter() });

let wrapper: Enzyme.ShallowWrapper;
const handleDialogClose = () => {
  console.log("hello");
};

describe("Components / Dialog / DialogTitle", () => {
  beforeEach(() => {
    wrapper = shallow(
      <DialogTitle id="customized-dialog-title" onClose={handleDialogClose}>
        SELECT ASSET
      </DialogTitle>
    );
  });

  it("renders", () => {
    expect(wrapper).toMatchSnapshot();
  });
  it("should be correct props", () => {
    expect(wrapper.prop("id")).toEqual("customized-dialog-title");
    expect(wrapper.prop("onClose")).toEqual(handleDialogClose);
  });
  it("should have one Typograph", () => {
    expect(wrapper.dive().find(Typography)).toHaveLength(1);
    expect(wrapper.dive().find(Typography).text()).toEqual("SELECT ASSET");
  });
  it("should have one IconButton", () => {
    expect(wrapper.dive().find(IconButton)).toHaveLength(1);
  });
});
