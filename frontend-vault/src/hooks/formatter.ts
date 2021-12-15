export const percentageFormatter = (amount: number): string => {
  const option = {
    style: "percent",
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  }

  return new Intl.NumberFormat("en-US", option).format(amount)
}
