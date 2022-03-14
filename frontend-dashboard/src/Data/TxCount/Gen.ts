import { Arbitrary, nat } from "fast-check"
import * as TxCount from "Data/TxCount"

export const genTxCount: Arbitrary<TxCount.Type> = nat().map(TxCount.iso.wrap)
