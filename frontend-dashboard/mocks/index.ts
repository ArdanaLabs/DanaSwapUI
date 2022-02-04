import path from "path"
import { readFile } from "fs/promises"

import * as TE from "fp-ts/TaskEither"

function mkReadTask(filename: string): TE.TaskEither<Error, unknown> {
  return TE.tryCatch(
    () =>
      readFile(path.resolve(__dirname, filename)).then((f) => {
        try {
          const j = JSON.parse(f.toString())
          return Promise.resolve(j as unknown)
        } catch (e) {
          return Promise.reject(e)
        }
      }),
    (e) => e as Error
  )
}

export const mocks: Record<string, TE.TaskEither<Error, unknown>> = {
  combined: mkReadTask("combined.json"),
  fiveMinutes: mkReadTask("five-minutes.json"),
  oneDay: mkReadTask("one-day.json"),
  oneMinute: mkReadTask("one-minute.json"),
  poolAPY: mkReadTask("pool-apy.json"),
  poolFees: mkReadTask("pool-fees.json"),
  poolLiquidity: mkReadTask("pool-liquidity.json"),
  poolTxCount: mkReadTask("pool-tx_count.json"),
  poolVolume: mkReadTask("pool-volume.json"),
  transactionsPool: mkReadTask("transactions-pool.json"),
}

export default mocks
