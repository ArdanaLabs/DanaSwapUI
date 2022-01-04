import React, { Suspense } from "react"
import { BrowserRouter, Switch, Route, Redirect } from "react-router-dom"
import { Provider as StateProvider } from "react-redux"
import {
  ThemeProvider as MuiThemeProvider,
  CssBaseline,
} from "@material-ui/core"
import { useIsDarkMode } from "state/user/hooks"
import { darkTheme, lightTheme } from "./theme"
import store from "./state"

import {
  Landing,
  TeamPage,
  MainLogoPage,
  BrandAssetsPage,
  TechPage,
  RoadMapPage,
  CommunityPage,
  NewsPage,
} from "./pages"
import Layout from "layouts/Layout"

const ThemeProvider: React.FC = ({ children }) => {
  const darkMode = useIsDarkMode()
  let theme = darkMode ? darkTheme : lightTheme

  return <MuiThemeProvider theme={theme}>{children}</MuiThemeProvider>
}

const Providers: React.FC = ({ children }) => {
  return (
    <BrowserRouter basename="/">
      <Suspense fallback={null}>
        <StateProvider store={store}>
          <ThemeProvider>
            <CssBaseline />
            <>{children}</>
          </ThemeProvider>
        </StateProvider>
      </Suspense>
    </BrowserRouter>
  )
}

const App: React.FC = () => {
  return (
    <Providers>
      <Switch>
        <Route exact path="/">
          <Layout>
            <Landing />
          </Layout>
        </Route>

        <Route exact path="/team">
          <Layout>
            <TeamPage />
          </Layout>
        </Route>

        <Route exact path="/brandassets">
          <Layout>
            <BrandAssetsPage />
          </Layout>
        </Route>

        <Route exact path="/tech">
          <Layout>
            <TechPage />
          </Layout>
        </Route>

        <Route exact path="/roadmap">
          <Layout>
            <RoadMapPage />
          </Layout>
        </Route>

        <Route exact path="/community">
          <Layout>
            <CommunityPage />
          </Layout>
        </Route>

        <Route exact path="/news">
          <Layout>
            <NewsPage />
          </Layout>
        </Route>

        <Route exact path="/mainlogo">
          <MainLogoPage />
        </Route>

        <Route path="*">
          <Redirect to="/" />
        </Route>
      </Switch>
    </Providers>
  )
}

export default App
