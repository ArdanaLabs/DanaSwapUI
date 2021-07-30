import _ from 'lodash'

export const extractXAxis = (arr: any[]): string[] => {
  const length = arr.length
  let newAxis: string[] = []

  newAxis.push(arr[0].start)
  for (let i = 0; i < length; i++) {
    newAxis.push(arr[i].end)
  }
  return newAxis
}

export const extractYAxis = (arr: any[], filter: string): any[] => {
  const length = arr.length
  let newAxis: any[] = []

  for (let i = 0; i < length; i++) {
    newAxis.push(arr[i][filter])
  }

  return newAxis
}

const fn = (obj: any, key: string): any => {
  if (_.has(obj, key))
    // or just (key in obj)
    return [obj]
  // elegant:
  return _.flatten(
    _.map(obj, function (v) {
      return typeof v == 'object' ? fn(v, key) : []
    })
  )
}

export const findKeyFromObject = (obj: any, key: string): any => {
  return fn(obj, key)[0] ? fn(obj, key)[0][key] : undefined
}