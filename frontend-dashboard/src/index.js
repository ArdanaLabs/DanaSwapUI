import React from "react"
import ReactDOM from "react-dom"
import "animate.css/animate.compat.css"
import "react-calendar/dist/Calendar.css"
import App from "./App"
import reportWebVitals from "./reportWebVitals"

import "./index.css"

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
