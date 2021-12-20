export const percentageFormatter = (amount: number): string => {
  const option = {
    style: "percent",
    maximumFractionDigits: 2,
  }

  return new Intl.NumberFormat("en-US", option).format(amount)
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
