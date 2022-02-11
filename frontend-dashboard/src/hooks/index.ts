import { API_URL } from "config/endpoints"

export * from "./lang"
export * from "./formatter"
export * from "./extractor"

const quotedRegex = /^"?(\w+)"?$/

export function wrapWithStraightQuotationMarks(s: string): string {
  return s.replace(quotedRegex, '"$1"')
}

export function apiURL(
  url: URL | string,
  params?: Record<string, string> | [string, string][] | string
): URL {
  const u = new URL(url, API_URL)
  if (params != null) {
    const usp: URLSearchParams = new URLSearchParams(params)
    u.search = usp.toString()
  }
  return u
}
