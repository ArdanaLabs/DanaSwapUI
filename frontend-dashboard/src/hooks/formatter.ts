export function nFormatter(num: number | null, digits: number = 0) {
  return new Intl.NumberFormat(undefined, {
    currency: "USD",
    currencyDisplay: "narrowSymbol",
    style: "currency",
    notation: "compact",
    minimumFractionDigits: digits,
  }).format(num ?? 0)
}

export function nReader(
  formatted: string,
  digits: number,
  space: boolean
): number | null {
  let cur = 0
  if (formatted.charAt(0) !== "$") {
    return null
  }
  cur += 1
  if (
    (space && formatted.charAt(1) !== " ") ||
    (!space && formatted.charAt(1) === " ")
  ) {
    return null
  }
  if (space) {
    cur += 1
  }

  const symbol = formatted.charAt(formatted.length - 1)
  let value

  if (parseInt(symbol) >= 0 && parseInt(symbol) <= 9) {
    value = parseInt(formatted.substring(cur))
  } else {
    value = parseInt(formatted.substring(cur, formatted.length - 1))
    switch (symbol) {
      case "K":
        value *= 10 ** 3
        break
      case "M":
        value *= 10 ** 6
        break
      case "G":
        value *= 10 ** 9
        break
      case "T":
        value *= 10 ** 12
        break
      case "P":
        value *= 10 ** 15
        break
      case "E":
        value *= 10 ** 18
        break
      default:
        value = null
        break
    }
  }

  return value
}
