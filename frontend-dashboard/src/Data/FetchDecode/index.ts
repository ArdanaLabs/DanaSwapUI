import { FetchError } from "fp-fetch"
import { Either } from "fp-ts/Either"
import { TaskEither } from "fp-ts/TaskEither"
import * as IO from "io-ts"

export type FetchDecodeError = IO.Errors | FetchError<Error>

export type FetchDecodeResult<A> = Either<FetchDecodeError, A>

export type FetchDecodeTask<A> = TaskEither<FetchDecodeError, A>
