import * as O from "fp-ts/Option"

export const API_URL: URL = O.getOrElse(
  () => new URL("https://stats.ardana.org")
)(
  O.tryCatch(() => {
    const u: URL = new URL(`${process.env.DANA_SWAP_STATS_HOST}`)
    u.port = process.env.DANA_SWAP_STATS_PORT ?? "8080"
    return u
  })
)
