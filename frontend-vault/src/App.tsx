import {
  CssBaseline,
  ThemeProvider as MuiThemeProvider,
} from "@material-ui/core"
import Layout from "layouts/Layout"
import React, { Suspense } from "react"
import { I18nextProvider } from "react-i18next"
import { Provider as StateProvider } from "react-redux"
import { BrowserRouter, Redirect, Route, Switch } from "react-router-dom"
import { ParallaxProvider } from "react-scroll-parallax"
import UiUpdater from "state/ui/updater"
import VaultUpdater from "state/vault/updater"
import WalletUpdater from "state/wallet/updater"
import { useIsDarkMode } from "state/user/hooks"
import i18n from "./i18n"
import {
  Landing,
  MyVaults,
  Vaults,
  CreateVaultMultiply,
  CreateVaultBorrow,
} from "./pages"
import store from "./state"
import { darkTheme, lightTheme } from "./theme"

const StateUpdaters: React.FC = () => (
  <>
    <UiUpdater />
    <VaultUpdater />
    <WalletUpdater />
  </>
)

const ThemeProvider: React.FC = ({ children }) => {
  const darkMode = useIsDarkMode()
  let theme = darkMode ? darkTheme : lightTheme

  return <MuiThemeProvider theme={theme}>{children}</MuiThemeProvider>
}

const Providers: React.FC = ({ children }) => (
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

const App: React.FC = () => (
  <Providers>
    <Switch>
      <Route exact path="/">
        <Layout>
          <Landing />
        </Layout>
      </Route>
      <Route exact path="/owner">
        <Layout>
          <MyVaults />
        </Layout>
      </Route>
      <Route exact path="/vaults/list">
        <Layout>
          <Vaults />
        </Layout>
      </Route>

      <Route exact path="/vaults/open-multiply/:type">
        <Layout>
          <CreateVaultMultiply />
        </Layout>
      </Route>

      <Route exact path="/vaults/open-borrow/:type">
        <Layout>
          <CreateVaultBorrow />
        </Layout>
      </Route>

      <Route path="*">
        <Redirect to="/" />
      </Route>
    </Switch>
  </Providers>
)
export default App
