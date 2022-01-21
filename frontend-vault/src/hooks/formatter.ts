import BigNumber from "bignumber.js"

export const percentageFormatter = (
  amount: number,
  digits: number = 2
): string => {
  const option = {
    style: "percent",
    minimumFractionDigits: digits,
    maximumFractionDigits: digits,
  }

  return new Intl.NumberFormat("en-US", option).format(amount)
}

export const currencyConvertor = (
  amount: BigNumber,
  decimals: number
): BigNumber => {
  return amount.dividedBy(Math.pow(10, decimals))
}

export const currencyFormatter = (
  amount: number,
  digits: number = 2
): string => {
  const option = {
    currency: "USD",
    currencyDisplay: "narrowSymbol",
    style: "currency",
    // notation: "compact",
    maximumFractionDigits: digits,
  }

  return new Intl.NumberFormat("en-US", option).format(amount)
}

export const numberFormatter = (amount: number, digits: number = 2): string => {
  const option = {
    notation: "compact" as
      | "compact"
      | "standard"
      | "scientific"
      | "engineering"
      | undefined,
    maximumFractionDigits: digits,
  }

  return new Intl.NumberFormat("en-US", option).format(amount)
}
