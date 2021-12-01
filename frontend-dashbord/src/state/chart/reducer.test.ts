import reducer, { initialState } from './reducer'
import {
  RangedAPY,
  RangedFees,
  RangedLiquidity,
  RangedTransactions,
  RangedTxCount,
  RangedVolume,
  updateAggLiquidity,
  updateAggVolume,
  updatePoolAPY,
  updatePoolFees,
  updatePoolLiquidity,
  updatePoolTransactions,
  updatePoolTXCount,
  updatePoolVolume
} from './actions'

describe('Chart reducers', () => {
  it('should return aggVolume when dispatch updateAggVolume action', () => {
    let mock: RangedVolume[] = []
    mock.push({
      start: null,
      end: null,
      addLiquidity: null,
      removeLiquidity: null,
      total: null,
      trade: null
    })

    expect(reducer(initialState, updateAggVolume(mock))).toStrictEqual({
      ...initialState,
      aggVolume: mock
    })
  })

  it('should return aggLiquidity when dispatch updateAggLiquidity action', () => {
    let mock: RangedLiquidity[] = []
    mock.push({
      start: null,
      end: null,
      value: null
    })

    expect(reducer(initialState, updateAggLiquidity(mock))).toStrictEqual({
      ...initialState,
      aggLiquidity: mock
    })
  })

  it('should return poolFees when dispatch updatePoolFees action', () => {
    let mock: RangedFees[] = []
    mock.push({
      start: null,
      end: null,
      value: null
    })

    expect(reducer(initialState, updatePoolFees(mock))).toStrictEqual({
      ...initialState,
      poolFees: mock
    })
  })

  it('should return poolVolume when dispatch updatePoolVolume action', () => {
    let mock: RangedVolume[] = []
    mock.push({
      start: null,
      end: null,
      addLiquidity: null,
      removeLiquidity: null,
      total: null,
      trade: null
    })

    expect(reducer(initialState, updatePoolVolume(mock))).toStrictEqual({
      ...initialState,
      poolVolume: mock
    })
  })

  it('should return poolLiquidity when dispatch updatePoolLiquidity action', () => {
    let mock: RangedLiquidity[] = []
    mock.push({
      start: null,
      end: null,
      value: null
    })

    expect(reducer(initialState, updatePoolLiquidity(mock))).toStrictEqual({
      ...initialState,
      poolLiquidity: mock
    })
  })

  it('should return poolTXCount when dispatch updatePoolTXCount action', () => {
    let mock: RangedTxCount[] = []
    mock.push({
      start: null,
      end: null,
      addLiquidity: null,
      removeLiquidity: null,
      total: null,
      trade: null
    })

    expect(reducer(initialState, updatePoolTXCount(mock))).toStrictEqual({
      ...initialState,
      poolTXCount: mock
    })
  })

  it('should return poolAPY when dispatch updatePoolAPY action', () => {
    let mock: RangedAPY[] = []
    mock.push({
      start: null,
      end: null,
      value: null
    })

    expect(reducer(initialState, updatePoolAPY(mock))).toStrictEqual({
      ...initialState,
      poolAPY: mock
    })
  })

  it('should return poolTransactions when dispatch updatePoolTransactions action', () => {
    let mock: RangedTransactions[] = []
    mock.push({
      tx: {
        tag: null,
        contents: {
          counterpartyAddress: null,
          created: null,
          spentAsset: null,
          purchasedAsset: null,
          spentAmount: null,
          purchasedAmount: null,
          amounts: {
            token: null
          }
        }
      },
      navUSD: null
    })

    expect(reducer(initialState, updatePoolTransactions(mock))).toStrictEqual({
      ...initialState,
      poolTransactions: mock
    })
  })
})
