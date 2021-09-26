import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import configureMockStore, { MockStoreEnhanced } from "redux-mock-store";
import { shallow } from "enzyme";
import { Provider, useSelector } from "react-redux";
import { initialState } from "state/user/reducer";
import { StatBox } from ".";

Enzyme.configure({ adapter: new Adapter() });

const mockDispatch = jest.fn();
jest.mock("react", () => ({
  ...jest.requireActual("react"),
  useEffect: jest.fn((f) => f()),
}));
jest.mock("react-redux", () => ({
  ...jest.requireActual("react-redux"),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch,
}));
const mockStore = configureMockStore([]);

let store: MockStoreEnhanced<unknown>;

describe("Components / Box / StatBox", () => {
  beforeEach(() => {
    store = mockStore(initialState);
    (useSelector as jest.Mock).mockImplementation((callback) => {
      return callback(initialState);
    });
  });

  afterEach(() => {
    (useSelector as jest.Mock).mockClear();
  });

  it("renders", () => {
    const wrapper = shallow(
      <Provider store={store}>
        <StatBox
          image={""}
          title="TOTAL VALUE LOCKED"
          content="$1,234,567"
          delay={0}
        />
      </Provider>
    );
    expect(wrapper).toMatchSnapshot();
  });
});
