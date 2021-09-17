import jsc from 'jsverify'
import { RangedLiquidity, RangedVolume } from 'state/chart/actions'
import { extractXAxis, extractYAxis, findKeyFromObject } from './extractor'

const mock: RangedVolume[] = [
  {
    start: '1',
    end: '2',
    addLiquidity: null,
    removeLiquidity: null,
    total: 23,
    trade: null
  },
  {
    start: '2',
    end: '3',
    addLiquidity: null,
    removeLiquidity: null,
    total: 56,
    trade: null
  }
]

const mockObject = {
  foo: {
    bar: 1
  }
}

const RangedVolumeType = jsc.record({
  start: jsc.oneof([jsc.string, jsc.constant(null)]),
  end: jsc.oneof([jsc.string, jsc.constant(null)]),
  addLiquidity: jsc.oneof([jsc.string, jsc.constant(null)]),
  removeLiquidity: jsc.oneof([jsc.string, jsc.constant(null)]),
  total: jsc.oneof([jsc.string, jsc.constant(null)]),
  trade: jsc.oneof([jsc.string, jsc.constant(null)])
}) as jsc.Arbitrary<RangedVolume>

const RangedLiquidityType = jsc.record({
  start: jsc.oneof([jsc.string, jsc.constant(null)]),
  end: jsc.oneof([jsc.string, jsc.constant(null)]),
  value: jsc.oneof([jsc.integer, jsc.constant(null)])
}) as jsc.Arbitrary<RangedLiquidity>

describe('Hooks extractor.ts', () => {
  describe('extractXAxis method', () => {
    const newAxis = extractXAxis(mock)

    it('should return string array type', () => {
      expect(newAxis).toBeInstanceOf(Array)
    })

    it('should return expected string array', () => {
      const expected = ['1', '2', '3']
      expect(newAxis).toEqual(expected)
    })

    it('should check param property', () => {
      jsc.check(
        jsc.forall(
          jsc.oneof([
            jsc.array(RangedVolumeType),
            jsc.array(RangedLiquidityType)
          ]),
          arg0 => {
            return Array.isArray(extractXAxis(arg0))
          }
        )
      )
    })
  })

  describe('extractYAxis method', () => {
    const newAxis = extractYAxis(mock, 'total')

    it('should return string array type', () => {
      expect(newAxis).toBeInstanceOf(Array)
    })

    it('should return expected string array', () => {
      const expected = [23, 56]
      expect(newAxis).toEqual(expected)
    })

    it('should check param property', () => {
      jsc.check(
        jsc.forall(
          jsc.oneof([
            jsc.array(RangedVolumeType),
            jsc.array(RangedLiquidityType)
          ]),
          jsc.string,
          (arg0, arg1) => {
            return Array.isArray(extractYAxis(arg0, arg1))
          }
        )
      )
    })
  })

  describe('findKeyFromObject method', () => {
    const result = findKeyFromObject(mockObject, 'bar')

    it('should return expected number', () => {
      const expected = 1
      expect(result).toEqual(expected)
    })

    it('should check param property', () => {
      jsc.check(
        jsc.forall(
          jsc.json,
          jsc.string,
          (arg0, arg1) => {
            findKeyFromObject(arg0, arg1)
            return true;
          }
        )
      )
    })
  })
})
