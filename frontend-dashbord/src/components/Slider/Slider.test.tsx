import { Mark } from "@material-ui/core";
import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import { shallow } from "enzyme";
import Slider from ".";

Enzyme.configure({ adapter: new Adapter() });

let wrapper: Enzyme.ShallowWrapper;
const onAmountChange = jest.fn();
const marks: Mark[] = [
  {
    value: 0,
    label: "0%",
  },
  {
    value: 100,
    label: "100%",
  },
];

describe("Components / Slider / Slider", () => {
  beforeEach(() => {
    wrapper = shallow(
      <Slider
        min={0}
        max={100}
        defaultValue={0}
        value={12}
        onChange={onAmountChange}
        step={1}
        marks={marks}
      />
    );
  });

  it("renders", () => {
    expect(wrapper).toMatchSnapshot();
  });
  it("should be correct props", () => {
    expect(wrapper.prop("min")).toEqual(0);
    expect(wrapper.prop("max")).toEqual(100);
    expect(wrapper.prop("defaultValue")).toEqual(0);
    expect(wrapper.prop("value")).toEqual(12);
    expect(wrapper.prop("onChange")).toEqual(onAmountChange);
    expect(wrapper.prop("step")).toEqual(1);
    expect(wrapper.prop("marks")).toEqual(marks);
  });
});
