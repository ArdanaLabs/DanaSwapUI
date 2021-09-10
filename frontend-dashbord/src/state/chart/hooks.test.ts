import { RangedVolume, updateAggVolume } from './actions'
import { useAggLiquidity, useAggVolume, usePoolAPY, usePoolFees, usePoolLiquidity, usePoolTransactions, usePoolTxCount, usePoolVolume } from './hooks'
import reducer, { initialState } from './reducer'
import { Provider } from 'react-redux'
import { mount, shallow } from 'enzyme'
// import { useSelector, useDispatch } from 'react-redux';

import * as redux from 'react-redux'
import configureMockStore from 'redux-mock-store'
import thunk from 'redux-thunk'
import App from 'App'
import { configureStore } from '@reduxjs/toolkit'

const mockDispatch = jest.fn()
const mockSelector = jest.fn()
jest.mock('react-redux', () => ({
  useSelector: () => mockSelector,
  useDispatch: () => mockDispatch
}))

// function setup() {
//   const store = configureStore();
//   const history = createBrowserHistory();
//   const provider = (
//     <Provider store={store}>
//       <ConnectedRouter history={history}>
//         <CounterPage />
//       </ConnectedRouter>
//     </Provider>
//   );
//   const app = mount(provider);
//   return {};
// }

const spy = jest.spyOn(redux, 'useSelector')
spy.mockReturnValue({ chart: { aggVolume: 'asdf' } })

let updatedState: any;

describe('Chart hooks', () => {
  beforeEach(() => {
    let mock: RangedVolume[] = []
    mock.push({
      start: null,
      end: null,
      addLiquidity: null,
      removeLiquidity: null,
      total: null,
      trade: null
    })
    updatedState = reducer(initialState, updateAggVolume(mock))

  })

  it('should return stored aggVolume and getAggVolume function', () => {
    const { aggVolume } = useAggVolume()

    expect(aggVolume).toEqual(updatedState.aggVolume)
  })
  
  it('should return stored aggLiquidity and getAggLiquidity function', () => {
    const { aggLiquidity } = useAggLiquidity()

    expect(aggLiquidity).toEqual(updatedState.aggLiquidity)
  })
  
  it('should return stored poolFees and getPoolFees function', () => {
    const { poolFees } = usePoolFees()

    expect(poolFees).toEqual(updatedState.poolFees)
  })
  
  it('should return stored poolVolume and getPoolVolume function', () => {
    const { poolVolume } = usePoolVolume()

    expect(poolVolume).toEqual(updatedState.poolVolume)
  })
  
  it('should return stored poolLiquidity and getPoolLiquidity function', () => {
    const { poolLiquidity } = usePoolLiquidity()

    expect(poolLiquidity).toEqual(updatedState.poolLiquidity)
  })

  it('should return stored poolTXCount and getPoolTXCount function', () => {
    const { poolTXCount } = usePoolTxCount()

    expect(poolTXCount).toEqual(updatedState.poolTXCount)
  })
  
  it('should return stored poolAPY and getPoolAPY function', () => {
    const { poolAPY } = usePoolAPY()

    expect(poolAPY).toEqual(updatedState.poolAPY)
  })
  
  it('should return stored poolTransactions and getPoolTransactions function', () => {
    const { poolTransactions } = usePoolTransactions()

    expect(poolTransactions).toEqual(updatedState.poolTransactions)
  })
})
