import { Arbitrary, record } from "fast-check"
import { ByTxType } from "Data/ByTxType"

export function genByTxType<A>(genA: Arbitrary<A>): Arbitrary<ByTxType<A>> {
  return record({
    trade: genA,
    addLiquidity: genA,
    removeLiquidity: genA,
    total: genA,
  })
}
