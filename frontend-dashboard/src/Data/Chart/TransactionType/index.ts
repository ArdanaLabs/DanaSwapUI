import * as Eq_ from "fp-ts/Eq"
import * as Show_ from "fp-ts/Show"

/**
 * @category model
 */
export enum TransactionType {
  Any = "Any",
  Trade = "Trade",
  Withdrawal = "Withdrawal",
  Deposit = "Deposit",
}

/**
 * @category instances
 */
export const Eq: Eq_.Eq<TransactionType> = {
  equals: (a, b) => a === b,
}

export const Show: Show_.Show<TransactionType> = {
  show: (tt) => tt,
}
