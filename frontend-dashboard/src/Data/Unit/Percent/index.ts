import * as Newtype from "newtype-ts"
import * as Show_ from "fp-ts/Show"
import * as number from "fp-ts/number"
import * as IO from "io-ts"
import { fromNewtype } from "io-ts-types"

export interface Percent
  extends Newtype.Newtype<{ readonly Percent: unique symbol }, number> {}

export type Type = Percent

export const codec: IO.Type<Percent, number> = fromNewtype<Percent>(IO.number)

export const iso = Newtype.iso<Percent>()

export const Eq = Newtype.getEq<Percent>(number.Eq)

export const Ord = Newtype.getOrd<Percent>(number.Ord)

export const Show: Show_.Show<Percent> = {
  show: (p) => `Percent(${iso.unwrap(p)})`,
}

// instance Validity Percent where
//   validate percent = mconcat
//     [ genericValidate percent
//     , declare "Percent is finite" $ not . isInfinite $ unPercent percent
//     , declare "Percent is not NaN" $ not . isNaN $ unPercent percent
//     ]
