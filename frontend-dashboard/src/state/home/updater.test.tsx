import { mount } from 'enzyme'
import * as Enzyme from 'enzyme'
import Adapter from '@wojtekmaj/enzyme-adapter-react-17'
import configureMockStore from 'redux-mock-store'
import HomeUpdater from './updater'
import React, { useEffect } from 'react'

Enzyme.configure({ adapter: new Adapter() })
const mockStore = configureMockStore([])

const initialState = {
  home: {
    totalDepositsAllPoolsUSD: null,
    totalDailyVolumeUSD: null,
    totalDailyFeeVolumeUSD: null,
    totalLiquidityUtilization: null,
    poolStats: null
  }
}

const mockDispatch = jest.fn()
jest.mock('react', () => ({
  ...jest.requireActual('react'),
  useEffect: jest.fn(f => f())
}))
jest.mock('react-redux', () => ({
  ...jest.requireActual('react-redux'),
  useSelector: jest.fn(),
  useDispatch: () => mockDispatch
}))
jest.useFakeTimers()

beforeEach(() => {
  mockStore(initialState)
})

describe('Home updater', () => {
  beforeEach(() => {
    mount(<HomeUpdater />)
  })

  it('should call useEffect', () => {
    expect(useEffect).toHaveBeenCalled()
  })

  // it('should update total stats every x seconds', async () => {
  //   expect(mockDispatch).not.toBeCalled()

  //   jest.advanceTimersByTime(1000 * 60 * 100 * 3)

  //   expect(mockDispatch).toHaveBeenCalledTimes(3)
  // })
})
