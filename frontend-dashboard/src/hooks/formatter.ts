export const printCurrencyUSD = (function () {
  let formatter: any
  return function (num: number | null, digits: number = 0): string {
    if (!(formatter instanceof Intl.NumberFormat)) {
      formatter = new Intl.NumberFormat(undefined, {
        currency: "USD",
        currencyDisplay: "narrowSymbol",
        style: "currency",
        notation: "compact",
        minimumFractionDigits: digits,
      })
    }
    return formatter.format(num ?? 0)
  }
})()

export function nReader(formatted: string, digits: number): number | null {
  let cur = 0
  if (formatted.charAt(0) !== "$" || formatted.trim() === "") {
    return null
  }
  cur += 1

  const symbol = formatted.charAt(formatted.length - 1)
  let value

  if (parseInt(symbol, 10) >= 0 && parseInt(symbol, 10) <= 9) {
    value = parseInt(formatted.substring(cur), 10)
  } else {
    value = parseInt(formatted.substring(cur, formatted.length - 1), 10)
    switch (symbol) {
      case "K":
        value *= 10 ** 3
        break
      case "M":
        value *= 10 ** 6
        break
      case "B":
        value *= 10 ** 9
        break
      case "T":
        value *= 10 ** 12
        break
      default:
        value = null
        break
    }
  }

  return value
}
