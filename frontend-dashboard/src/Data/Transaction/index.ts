import * as IO from "io-ts"
import { mapper } from "io-ts-extra"

import * as Aeson from "Data/Aeson"
import * as Transfer from "Data/Transfer"
import * as Trade_ from "Data/Trade"
import { USD } from "Data/Unit"

import * as Trade from "./Trade"
import * as Deposit from "./Deposit"
import * as Withdrawal from "./Withdrawal"

export * as Trade from "./Trade"
export * as Deposit from "./Deposit"
export * as Withdrawal from "./Withdrawal"

/*
 * @category model
 */

export type Transaction = Trade.Type | Deposit.Type | Withdrawal.Type

export type Type = Transaction

export const codec: IO.Type<Transaction, object> = IO.union([
  Trade.codec,
  Deposit.codec,
  Withdrawal.codec,
])

function adaptFromAeson(tx: Aeson.AesonTagged) {
  switch (tx.tag) {
    case "TradeTx":
      return { _tag: "Trade", trade: tx.contents }
    case "DepositTx":
      return { _tag: "Deposit", deposit: tx.contents }
    case "WithdrawalTx":
      return { _tag: "Withdrawal", withdrawal: tx.contents }
    default:
      throw new Error(`Unsupported tag “${tx.tag}”`)
  }
}

function adaptToAeson(tx: Transaction): Aeson.AesonTagged {
  switch (tx._tag) {
    case "Trade":
      return { tag: "TradeTx", contents: tx.trade }
    case "Deposit":
      return { tag: "DepositTx", contents: tx.deposit }
    case "Withdrawal":
      return { tag: "WithdrawalTx", contents: tx.withdrawal }
  }
}

export const transactionAdaptedFromAeson: IO.Type<
  Transaction,
  Aeson.AesonTagged
> = mapper(Aeson.codec, codec, adaptFromAeson, adaptToAeson)

export const codecWithTotalValue = IO.strict(
  {
    transaction: codec,
    navUSD: USD.codec,
  },
  "TxWithTotalValue"
)

export type WithTotalValue = IO.TypeOf<typeof codecWithTotalValue>

export const withTotalValueAdaptedFromAeson = IO.intersection([
  mapper(
    IO.type({ tx: Aeson.codec }),
    IO.type({ transaction: codec }),
    function to(data) {
      return { transaction: adaptFromAeson(data.tx) }
    },
    function from(data) {
      return { tx: adaptToAeson(data.transaction) }
    }
  ),
  IO.type({ navUSD: USD.codec }),
])

/*
 * @category constructor
 */

export function trade(trade_: Trade_.Trade): Transaction {
  return { _tag: "Trade", trade: trade_ }
}

export function deposit(transferWithValue: Transfer.Type): Transaction {
  return { _tag: "Deposit", deposit: transferWithValue }
}

export function withdrawal(transferWithValue: Transfer.Type): Transaction {
  return { _tag: "Withdrawal", withdrawal: transferWithValue }
}
