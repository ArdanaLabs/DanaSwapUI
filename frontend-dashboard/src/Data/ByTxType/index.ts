// Back-end wraps all of these values in Newtypes,
// overkill for now as I’m unsure if any consuming functions care yet

import * as IO from "io-ts"

export const URI_ = "ByTxType"

export type URI = typeof URI_

declare module "fp-ts/HKT" {
  interface URItoKind<A> {
    readonly [URI_]: ByTxType<A>
  }
}

export type ByTxType<A> = {
  readonly trade: A
  readonly addLiquidity: A
  readonly removeLiquidity: A
  readonly total: A
}

export type Type<A> = ByTxType<A>

export interface ByTxTypeC<C extends IO.Mixed>
  extends IO.Type<ByTxType<IO.TypeOf<C>>, ByTxType<IO.OutputOf<C>>, unknown> {}

export function codec<C extends IO.Mixed>(
  codec: C,
  name: string = `ByTxType<${codec.name}>`
): ByTxTypeC<C> {
  return IO.strict(
    {
      trade: codec,
      addLiquidity: codec,
      removeLiquidity: codec,
      total: codec,
    },
    name
  )
}
