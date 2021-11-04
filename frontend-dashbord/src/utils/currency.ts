import { Currency } from "state/wallet/actions";

export const currencyFormatter = (currency: Currency, decimal: number = 6) => {
  return {
    unit: currency.unit,
    quantity: currency.quantity.dividedBy(10 ** decimal)
  }
}