import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import { shallow } from "enzyme";
import SwitchWithGlider from ".";
import { Button } from "components/Button";

Enzyme.configure({ adapter: new Adapter() });

let wrapper: Enzyme.ShallowWrapper;
const handleSwitch = jest.fn();

describe("Components / SwitchWithClider / SwitchWithClider", () => {
  beforeEach(() => {
    wrapper = shallow(
      <SwitchWithGlider
        elements={["VOLUME", "TVL", "LIQUIDITY"]}
        normalClass={"normalClass"}
        activeClass={"activeClass"}
        activeIndex={0}
        handleSwitch={handleSwitch}
      />
    );
  });

  it("renders", () => {
    expect(wrapper).toMatchSnapshot();
  });
  it("should have three Button", () => {
    expect(wrapper.find(Button)).toHaveLength(3);
  });
  it("should have active status", () => {
    wrapper.find(Button).at(1).simulate("click");
    expect(handleSwitch).toHaveBeenCalledTimes(1);
    // expect(wrapper.find(Button).at(0).hasClass("activeClass")).toEqual(true);
  });
});
