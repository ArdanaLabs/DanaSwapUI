// Used to adapt Haskell’s Aeson output to something fp-ts-compatible
import * as IO from "io-ts"

export interface FpTsTagged {
  _tag: string
}

export interface AesonTagged {
  tag: string
  contents: unknown
}

export const codec = IO.strict({ tag: IO.string, contents: IO.unknown })

export type Codec = IO.TypeOf<typeof codec>
