// ReadonlyMap Type for io-ts
import { Either, isLeft } from "fp-ts/Either"
import {
  Context,
  DictionaryType,
  Errors,
  Mixed,
  TypeOf,
  UnknownRecordC,
  appendContext,
  failure,
  failures,
  success,
} from "io-ts"

export interface ReadonlyMapFromRecordC<D extends Mixed, C extends Mixed>
  extends DictionaryType<
    D,
    C,
    ReadonlyMap<TypeOf<D>, TypeOf<C>>,
    UnknownRecordC,
    unknown
  > {}

export function readonlyMapFromRecord<D extends Mixed, C extends Mixed>(
  domain: D,
  codomain: C,
  name: string = `ReadonlyMap<${domain.name}, ${codomain.name}>`
): ReadonlyMapFromRecordC<D, C> {
  return new DictionaryType(
    name,
    (u: unknown): u is ReadonlyMap<D, C> => {
      if (u == null || !(u instanceof Map)) {
        return false
      }
      for (const [k, v] of u) {
        if (!domain.is(k) || !codomain.is(v)) {
          return false
        }
      }
      return true
    },
    (u: unknown, c: Context): Either<Errors, ReadonlyMap<D, C>> => {
      if (u == null || typeof u !== "object") {
        return failure(u, c)
      }
      const a: Map<D, C> = new Map()
      let errors: Errors = []
      Object.entries(u).forEach(([k, v]: [string, any]) => {
        const domainResult = domain.validate(k, appendContext(c, k, domain, k))
        if (isLeft(domainResult)) {
          errors = errors.concat(domainResult.left)
        } else {
          const codomainResult = codomain.validate(
            v,
            appendContext(c, k, codomain, v)
          )
          if (isLeft(codomainResult)) {
            errors = errors.concat(codomainResult.left)
          } else {
            a.set(domainResult.right, codomainResult.right)
          }
        }
      })
      return errors.length > 0
        ? failures(errors)
        : success(a as ReadonlyMap<D, C>)
    },
    (a: ReadonlyMap<D, C>) => {
      const s: { [key: string]: any } = {}
      for (const [k, v] of a) {
        s[String(domain.encode(k))] = codomain.encode(v)
      }
      return s as any
    },
    domain,
    codomain
  )
}
