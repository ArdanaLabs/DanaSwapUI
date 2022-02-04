import React, { Suspense, useState, useEffect } from "react"
import { BrowserRouter, Switch, Route, Redirect } from "react-router-dom"
import { Provider as StateProvider } from "react-redux"
import {
  ThemeProvider as MuiThemeProvider,
  CssBaseline,
} from "@material-ui/core"
import { ParallaxProvider } from "react-scroll-parallax"
import { I18nextProvider } from "react-i18next"

import i18n from "./i18n"
import { useIsDarkMode } from "state/user/hooks"
import { darkTheme, lightTheme } from "./theme"
import store from "./state"

import * as PoolSetName from "Data/Pool/PoolSetName"

import {
  Home,
  Swap,
  Pools,
  DANA,
  Launch,
  SpecPool,
  Withdraw,
  Deposit,
} from "./pages"
import Layout from "layouts/Layout"

import HomeUpdater from "./state/home/updater"
import UserUpdater from "./state/user/updater"
// import WSUpdater from "./state/websocket/updater";
import ChartUpdater from "./state/chart/updater"

const StateUpdaters: React.FC = () => {
  return (
    <>
      <HomeUpdater />
      <UserUpdater />
      {/* <WSUpdater /> */}
      <ChartUpdater />
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
            <StateUpdaters />

            <ThemeProvider>
              <CssBaseline />
              <I18nextProvider i18n={i18n}>{children}</I18nextProvider>
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
          <Redirect to="/home" />
        </Route>

        <Route exact path="/home">
          <Layout>
            <Home />
          </Layout>
        </Route>

        <Route exact path="/swap">
          <Layout>
            <Swap />
          </Layout>
        </Route>

        <Route exact path="/withdraw">
          <Layout>
            <Withdraw />
          </Layout>
        </Route>

        <Route exact path="/deposit">
          <Layout>
            <Deposit />
          </Layout>
        </Route>

        <Route exact path="/pools">
          <Layout>
            <Pools />
          </Layout>
        </Route>

        <Route exact path="/dana">
          <Layout>
            <DANA />
          </Layout>
        </Route>

        <Route
          path="/spec/:poolSet"
          render={({ match }) => (
            <Layout>
              <SpecPool poolSet={PoolSetName.iso.wrap(match.params.poolSet)} />
            </Layout>
          )}
        />

        <Route exact path="/launch">
          <Launch />
        </Route>

        <Route path="*">
          <Redirect to="/home" />
        </Route>
      </Switch>
    </Providers>
  )
}

export default App
