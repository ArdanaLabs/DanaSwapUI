import _ from "lodash"
import { RangedLiquidity, RangedVolume } from "state/chart/actions"

export const extractXAxis = (
  arr: RangedVolume[] | RangedLiquidity[]
): string[] => {
  const length = arr.length
  let newAxis: string[] = []

  newAxis.push(arr[0]?.start ?? "")
  for (let i = 0; i < length; i++) {
    newAxis.push(arr[i]?.end ?? "")
  }
  return newAxis
}

export const extractYAxis = (
  arr: RangedVolume[] | RangedLiquidity[],
  filter: string
): any[] => {
  const length = arr.length
  let newAxis: string[] = []

  for (let i = 0; i < length; i++) {
    const item: RangedVolume | RangedLiquidity = arr[i]
    newAxis.push(_.get(item, [filter]))
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
      return typeof v == "object" ? fn(v, key) : []
    })
  )
}

export const findKeyFromObject = (obj: object, key: string): any => {
  return fn(obj, key)[0] ? fn(obj, key)[0][key] : undefined
}
