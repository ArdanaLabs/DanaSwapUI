export function nFormatter (
  num: number | null,
  digits: number = 0,
  space: boolean = true
) {
  if (num === null || num <= 0) {
    return `$${space ? ' ' : ''}0`
  }

  const lookup = [
    { value: 1, symbol: '' },
    { value: 1e3, symbol: 'K' },
    { value: 1e6, symbol: 'M' },
    { value: 1e9, symbol: 'G' },
    { value: 1e12, symbol: 'T' },
    { value: 1e15, symbol: 'P' },
    { value: 1e18, symbol: 'E' }
  ]
  const rx = /\.0+$|(\.[0-9]*[1-9])0+$/
  var item = lookup
    .slice()
    .reverse()
    .find(function (item) {
      return num >= item.value
    })
  return (
    '$' +
    (space ? ' ' : '') +
    (item
      ? (num / item.value).toFixed(digits).replace(rx, '$1') + item.symbol
      : '0')
  )
}
