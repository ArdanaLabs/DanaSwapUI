import jsc from "jsverify"
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

const RangedVolumeType = jsc.record({
  start: jsc.string,
  end: jsc.string,
  addLiquidity: jsc.string,
  removeLiquidity: jsc.string,
  total: jsc.string,
  trade: jsc.string,
})

const RangedLiquidityType = jsc.record({
  start: jsc.string,
  end: jsc.string,
  value: jsc.number
})

const RangedFeesType = jsc.record({
  start: jsc.string,
  end: jsc.string,
  value: jsc.number
})

const RangedTxCountType = jsc.record({
  start: jsc.string,
  end: jsc.string,
  addLiquidity: jsc.string,
  removeLiquidity: jsc.string,
  total: jsc.string,
  trade: jsc.string,
})

const RangedAPYType = jsc.record({
  start: jsc.string,
  end: jsc.string,
  value: jsc.number
})

// const RangedTransactions = jsc.record({
//   start: jsc.string,
//   end: jsc.string,
//   addLiquidity: jsc.string,
//   removeLiquidity: jsc.string,
//   total: jsc.string,
//   trade: jsc.string,
// })

describe('Chart actions', () => {
  describe('updateAggVolume', () => {
    it('should create action with passed data', () => {
      let mock: RangedVolume[] = []
      mock.push({
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      })

      expect(updateAggVolume(mock)).toEqual({
        type: 'home/updateAggVolume',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedVolumeType, updateAggVolume);
    })
  })

  describe('updateAggLiquidity', () => {
    it('should create action with passed data', () => {
      let mock: RangedLiquidity[] = []
      mock.push({
        start: null,
        end: null,
        value: null
      })

      expect(updateAggLiquidity(mock)).toEqual({
        type: 'home/updateAggLiquidity',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedLiquidityType, updateAggLiquidity);
    })
  })

  describe('updatePoolFees', () => {
    it('should create action with passed data', () => {
      let mock: RangedFees[] = []
      mock.push({
        start: null,
        end: null,
        value: null
      })

      expect(updatePoolFees(mock)).toEqual({
        type: 'home/updatePoolFees',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedFeesType, updatePoolFees);
    })
  })

  describe('updatePoolVolume', () => {
    it('should create action with passed data', () => {
      let mock: RangedVolume[] = []
      mock.push({
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      })

      expect(updatePoolVolume(mock)).toEqual({
        type: 'home/updatePoolVolume',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedVolumeType, updatePoolVolume);
    })
  })

  describe('updatePoolLiquidity', () => {
    it('should create action with passed data', () => {
      let mock: RangedLiquidity[] = []
      mock.push({
        start: null,
        end: null,
        value: null
      })

      expect(updatePoolLiquidity(mock)).toEqual({
        type: 'home/updatePoolLiquidity',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedLiquidityType, updatePoolLiquidity);
    })
  })

  describe('updatePoolTXCount', () => {
    it('should create action with passed data', () => {
      let mock: RangedTxCount[] = []
      mock.push({
        start: null,
        end: null,
        addLiquidity: null,
        removeLiquidity: null,
        total: null,
        trade: null
      })

      expect(updatePoolTXCount(mock)).toEqual({
        type: 'home/updatePoolTXCount',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedTxCountType, updatePoolTXCount);
    })
  })

  describe('updatePoolAPY', () => {
    it('should create action with passed data', () => {
      let mock: RangedAPY[] = []
      mock.push({
        start: null,
        end: null,
        value: null
      })

      expect(updatePoolAPY(mock)).toEqual({
        type: 'home/updatePoolAPY',
        payload: mock
      })
    })

    describe('basic jsverify usage', () => {
      jsc.checkForall(RangedAPYType, updatePoolAPY);
    })
  })

  describe('updatePoolTransactions', () => {
    it('should create action with passed data', () => {
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

      expect(updatePoolTransactions(mock)).toEqual({
        type: 'home/updatePoolTransactions',
        payload: mock
      })
    })

    // describe('basic jsverify usage', () => {
    //   jsc.checkForall(RangedTransactionType, updatePoolTransactions);
    // })
  })
})
