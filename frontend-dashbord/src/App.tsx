import React, { Suspense, useState, useEffect } from "react";
import { BrowserRouter, Switch, Route, Redirect } from "react-router-dom";
import { Provider as StateProvider } from "react-redux";
import {
  ThemeProvider as MuiThemeProvider,
  CssBaseline,
} from "@material-ui/core";
import { ParallaxProvider } from "react-scroll-parallax";
import { I18nextProvider } from "react-i18next";

import i18n from "./i18n";
import { useIsDarkMode } from "state/user/hooks";
import { darkTheme, lightTheme } from "./theme";
import store from "./state";

import { Home, Swap, Pools, Dao, DANA } from "./pages";
import Layout from "layouts/Layout";

import HomeUpdater from "./state/home/updater";
import UserUpdater from "./state/user/updater";

const StateUpdaters: React.FC = () => {
  return (
    <>
      <HomeUpdater />
      <UserUpdater />
    </>
  );
};

const ThemeProvider: React.FC = ({ children }) => {
  // const location = useLocation();
  const darkMode = useIsDarkMode();
  let theme = darkMode ? darkTheme : lightTheme;

  // if (location.pathname.replace('/', '') === '') {
  //   theme = darkTheme;
  // }

  return <MuiThemeProvider theme={theme}>{children}</MuiThemeProvider>;
};

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
  );
};

const App: React.FC = () => {
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    setLoading(false);
  }, []);

  if (loading) {
    return null;
  }

  return (
    <Providers>
      <Layout>
        <Switch>
          <Route exact path="/">
            {/* <Landing /> */}
            <Redirect to="/home" />
          </Route>

          <Route exact path="/home">
            <Home />
          </Route>

          <Route exact path="/swap">
            <Swap />
          </Route>

          <Route exact path="/pools">
            <Pools />
          </Route>

          <Route exact path="/dao">
            <Dao />
          </Route>

          <Route exact path="/dana">
            <DANA />
          </Route>

          <Route path="*">
            <Redirect to="/home" />
          </Route>
        </Switch>
      </Layout>
    </Providers>
  );
};

export default App;
