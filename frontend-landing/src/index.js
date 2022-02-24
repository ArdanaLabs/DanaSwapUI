import React from "react"
import ReactDOM from "react-dom"
import App from "./App"
import reportWebVitals from "./reportWebVitals"

import "./index.css"

Promise.all([
  // conditionally load HLS.js if user agent reports we can’t play streams
  document.createElement("video").canPlayType("application/vnd.apple.mpegurl")
    ? Promise.resolve()
    : import("hls.js"),
]).then(([Hls]) => {
  if (typeof Hls?.default === "function") {
    // By setting this ourselves, we prevent ReactPlayer’s runtime script
    // injection
    window.Hls = Hls.default
  }

  const rootEl: Element | null = document.getElementById("root")

  if (rootEl == null) {
    throw new Error("Could not mount. Root element `#root` missing.")
  }

  ReactDOM.render(
    <React.StrictMode>
      <App />
    </React.StrictMode>,
    rootEl
  )

  // If you want to start measuring performance in your app, pass a function
  // to log results (for example: reportWebVitals(console.log))
  // or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
  reportWebVitals()
})
