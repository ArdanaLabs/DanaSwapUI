import * as Option from "fp-ts/Option"
import _ from "lodash"
import { RangedLiquidity, RangedVolume } from "state/chart/actions"

// the strings are ISO8601 date strings
export const extractXAxis = ([head, ...tail]:
  | RangedVolume[]
  | RangedLiquidity[]): string[] => {
  return head == null
    ? ["", ""] // Safeguard
    : [head.start, head.end, ...tail.map((t) => t.end)].map(
        Option.getOrElse(() => "")
      )
}

export const extractYAxis = (
  arr: RangedVolume[] | RangedLiquidity[],
  filter: string
): string[] => {
  return arr.map((item) => Option.getOrElse(() => "")(_.get(item, [filter])))
}

export function findKeyFromObject(obj: object, key: string): any {
  const fn = (obj: any, key: string): any => {
    return obj != null && obj.hasOwnProperty(key)
      ? [obj]
      : Object.values(obj).flatMap((v) =>
          typeof v == "object" ? fn(v, key) : []
        )
  }
  return fn(obj, key)[0] ? fn(obj, key)[0][key] : undefined
}
