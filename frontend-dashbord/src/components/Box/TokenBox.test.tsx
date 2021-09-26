import { Box } from "@material-ui/core";
import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import { shallow } from "enzyme";
import { TokenBox } from ".";
import LOGO_Ardana from "assets/logos/ardana.png";
import { Dialog } from "components/Dialog";

Enzyme.configure({ adapter: new Adapter() });

jest.mock("react-redux", () => ({
  useDispatch: () => {},
  useSelector: () => ({
    user: {
      userDarkMode: null,
      mediaDarkMode: false,
      timestamp: "dfd",
    },
  }),
}));

let wrapper: Enzyme.ShallowWrapper;

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
    );
  });

  it("renders", () => {
    expect(wrapper).toMatchSnapshot();
  });
  it("should be correct props", () => {
    expect(wrapper.prop("className")).toEqual("someClassName");
    expect(wrapper.prop("style")).toEqual({ padding: 10 });
  });
  it("should contain someClassName", () => {
    expect(wrapper.find(".someClassName")).toHaveLength(1);
  });
  it("should contain two 22 Box component", () => {
    expect(wrapper.find(Box)).toHaveLength(22);
  });
  it("should contain 3 image tag", () => {
    expect(wrapper.find("img")).toHaveLength(3);
  });
  it("should contain correct label string", () => {
    expect(wrapper.text().includes("SEND")).toBeTruthy();
  });
  it("should contain Dialog component", () => {
    expect(wrapper.find(Dialog)).toHaveLength(1);
  });
});
