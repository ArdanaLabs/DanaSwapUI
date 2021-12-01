import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import { shallow } from "enzyme";
import VerticalCarousel from ".";

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

describe("Components / Carousel / VerticalCarousel", () => {
  beforeEach(() => {
    wrapper = shallow(
      <VerticalCarousel
        activeIndex={0}
        setActiveIndex={() => {}}
        data={["DANASWAP", `ARDANA VAULTS`, "MY DASHBOARD"]}
      />
    );
  });

  it("renders", () => {
    expect(wrapper).toMatchSnapshot();
  });
  it("should contain 3 button", () => {
    expect(wrapper.find('button')).toHaveLength(3);
  });
  it("should contain correct button labels", () => {
    expect(wrapper.find('button').at(0).text()).toEqual('DANASWAP');
    expect(wrapper.find('button').at(1).text()).toEqual('ARDANA VAULTS');
    expect(wrapper.find('button').at(2).text()).toEqual('MY DASHBOARD');
  });
});
