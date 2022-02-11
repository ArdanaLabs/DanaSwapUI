import * as Asset from "Data/Asset"
import { AssetQuantity, Percent, USD } from "Data/Unit"

export const printCurrencyUSD = (function () {
  const defaultOptions: Intl.NumberFormatOptions = {
    currencyDisplay: "narrowSymbol",
    notation: "compact",
    minimumFractionDigits: 0,
  }

  const mandatoryOptions: Intl.NumberFormatOptions = {
    currency: "USD",
    style: "currency",
  }

  const defaultFormatter: Intl.NumberFormat = new Intl.NumberFormat(undefined, {
    ...defaultOptions,
    ...mandatoryOptions,
  })

  return function (
    usd: USD.Type,
    options?: Omit<Intl.NumberFormatOptions, "style" | "currency">
  ): string {
    const formatter: Intl.NumberFormat =
      options == null
        ? defaultFormatter
        : new Intl.NumberFormat(undefined, {
            ...defaultOptions,
            ...options,
            ...mandatoryOptions,
          })

    return formatter.format(USD.iso.unwrap(usd))
  }
})()

// Legacy?
/*
export function nReader(
  formatted: string,
  _minimumFractionDigits: number
): number | null {
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
*/

export const printAssetQuantity = (function () {
  // This hack will be used later with a String.prototype.replace to get an
  // arbitrary currency as many digital currencies are not supported
  const hackCurrency: string = "USD"
  const hackRegex: RegExp = new RegExp(hackCurrency)

  const mandatoryOptions: Intl.NumberFormatOptions = {
    currency: hackCurrency,
    currencyDisplay: "code",
    style: "currency",
  }

  const defaultFormatter: Intl.NumberFormat = new Intl.NumberFormat(
    undefined,
    mandatoryOptions
  )

  return function (
    asset: Asset.Type,
    assetQuantity: AssetQuantity.Type,
    options?: Omit<
      Intl.NumberFormatOptions,
      "currency" | "currencyDisplay" | "style"
    >
  ): string {
    const formatter =
      options == null
        ? defaultFormatter
        : new Intl.NumberFormat(undefined, { ...options, ...mandatoryOptions })

    return formatter
      .format(AssetQuantity.iso.unwrap(assetQuantity))
      .replace(hackRegex, Asset.iso.unwrap(asset))
  }
})()

export const printPercentage = (function () {
  const defaultOptions: Intl.NumberFormatOptions = {
    maximumFractionDigits: 2,
  }

  const mandatoryOptions: Intl.NumberFormatOptions = {
    style: "percent",
  }

  const defaultFormatter: Intl.NumberFormat = new Intl.NumberFormat(undefined, {
    ...defaultOptions,
    ...mandatoryOptions,
  })

  return function (
    p: Percent.Type,
    options?: Omit<Intl.NumberFormatOptions, "style">
  ): string {
    const formatter: Intl.NumberFormat =
      options == null
        ? defaultFormatter
        : new Intl.NumberFormat(undefined, {
            ...defaultOptions,
            ...options,
            ...mandatoryOptions,
          })
    return formatter.format(Percent.iso.unwrap(p))
  }
})()

export const printDate = (function () {
  const defaultFormatter: Intl.DateTimeFormat = new Intl.DateTimeFormat(
    undefined,
    { dateStyle: "short" }
  )

  return function (d: Date, options?: Intl.DateTimeFormatOptions) {
    const formatter: Intl.DateTimeFormat =
      options == null
        ? defaultFormatter
        : new Intl.DateTimeFormat(undefined, options)

    return formatter.format(d)
  }
})()
