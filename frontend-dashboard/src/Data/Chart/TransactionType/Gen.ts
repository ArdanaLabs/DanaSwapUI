import { Arbitrary, oneof, constant } from "fast-check"
import { TransactionType } from "Data/Chart/TransactionType"

export const genTransaction: Arbitrary<TransactionType> = oneof(
  constant(TransactionType.Any),
  constant(TransactionType.Trade),
  constant(TransactionType.Withdrawal),
  constant(TransactionType.Deposit)
)
