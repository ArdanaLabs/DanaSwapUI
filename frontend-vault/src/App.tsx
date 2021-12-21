import {
  CssBaseline,
  ThemeProvider as MuiThemeProvider,
} from "@material-ui/core"
import Layout from "layouts/Layout"
import React, { Suspense, useEffect, useState } from "react"
import { I18nextProvider } from "react-i18next"
import { Provider as StateProvider } from "react-redux"
import { BrowserRouter, Redirect, Route, Switch } from "react-router-dom"
import { ParallaxProvider } from "react-scroll-parallax"
import { useIsDarkMode } from "state/user/hooks"
import i18n from "./i18n"
import { Landing, MyVaults, Vaults } from "./pages"
import store from "./state"
import { darkTheme, lightTheme } from "./theme"
import UiUpdater from "state/ui/updater"

const StateUpdaters: React.FC = () => {
  return (
    <>
      <UiUpdater />
    </>
  )
}

const ThemeProvider: React.FC = ({ children }) => {
  // const location = useLocation();
  const darkMode = useIsDarkMode()
  let theme = darkMode ? darkTheme : lightTheme

  // if (location.pathname.replace('/', '') === '') {
  //   theme = darkTheme;
  // }

  return <MuiThemeProvider theme={theme}>{children}</MuiThemeProvider>
}

const Providers: React.FC = ({ children }) => {
  return (
    <ParallaxProvider>
      <BrowserRouter basename="/">
        <Suspense fallback={null}>
          <StateProvider store={store}>
            <ThemeProvider>
              <CssBaseline />
              <I18nextProvider i18n={i18n}>{children}</I18nextProvider>
              <StateUpdaters />
            </ThemeProvider>
          </StateProvider>
        </Suspense>
      </BrowserRouter>
    </ParallaxProvider>
  )
}

const App: React.FC = () => {
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    setLoading(false)
  }, [])

  if (loading) {
    return null
  }

  return (
    <Providers>
      <Switch>
        <Route exact path="/">
          <Layout>
            <Landing />
          </Layout>
        </Route>
        <Route exact path="/myvaults">
          <Layout>
            <MyVaults />
          </Layout>
        </Route>
        <Route exact path="/vaults">
          <Layout>
            <Vaults />
          </Layout>
        </Route>
        <Route path="*">
          <Redirect to="/" />
        </Route>
      </Switch>
    </Providers>
  )
}

export default App
