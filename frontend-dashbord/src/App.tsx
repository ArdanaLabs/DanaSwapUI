import React, { Suspense, useState, useEffect } from "react";
import { BrowserRouter, Switch, Route } from "react-router-dom";
import { Provider as StateProvider } from "react-redux";
import {
  ThemeProvider as MuiThemeProvider,
  CssBaseline,
} from "@material-ui/core";
import { ParallaxProvider } from "react-scroll-parallax";
import { I18nextProvider } from "react-i18next";

import i18n from './i18n';
import { useIsDarkMode } from "state/user/hooks";
import { darkTheme, lightTheme } from "./theme";
import store from "./state";

import { Landing, Home, Swap, Pools, Dao } from "./pages";
import Layout from "layouts/Layout";

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
      <BrowserRouter>
        <Suspense fallback={null}>
          <StateProvider store={store}>
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
      <Switch>
        <Route exact path="/">
          <Landing />
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

        <Route exact path="/pools">
          <Layout>
            <Pools />
          </Layout>
        </Route>

        <Route exact path="/dao">
          <Layout>
            <Dao />
          </Layout>
        </Route>

        <Route path="*">
          <Layout>
            <Home />
          </Layout>
        </Route>
      </Switch>
    </Providers>
  );
};

export default App;
