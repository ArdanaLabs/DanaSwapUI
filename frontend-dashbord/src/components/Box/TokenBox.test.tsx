import Adapter from "@wojtekmaj/enzyme-adapter-react-17";
import * as Enzyme from "enzyme";
import configureMockStore, { MockStoreEnhanced } from "redux-mock-store";
import { Provider, useSelector } from "react-redux";
import { initialState } from "state/user/reducer";
import { TokenBox } from ".";

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

describe("Components / Box / TokenBox", () => {
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
    const wrapper = Enzyme.shallow(
      <Provider store={store}>
        <TokenBox
          label="SEND"
          token={"token"}
          amount={11}
          onMaxAmount={() => {}}
          handleTokenSelect={() => {}}
          className={""}
          style={{ padding: 10 }}
        />
      </Provider>
    );
    expect(wrapper).toMatchSnapshot();
  });
});
