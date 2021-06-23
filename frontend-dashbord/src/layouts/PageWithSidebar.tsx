import React, { useState, useEffect } from "react";
import {
  Box,
  Container,
  IconButton,
  Grid,
  Divider,
  useMediaQuery,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import Hamburger from "hamburger-react";
import cx from "classnames";

import { useDarkModeManager } from "state/user/hooks";

import MainLogo from "assets/svg/MainLogo.svg";
import Avatar from "assets/svg/Avatar.svg";
import { Sidebar } from "components";
import { Link } from "react-router-dom";

import Ardana_hor_logo from "assets/Ardana_hor color 1.svg";
import { Button } from "components/Button";

const useStyles = makeStyles(({ palette }) => ({
  page: {
    backgroundColor: palette.background.default,
    width: "calc(100vw - 230px)",
    position: "relative",
    minHeight: "100vh",
    display: "flex",
    overflowX: "hidden",
    flexDirection: "column",
    marginLeft: 230,
  },
  pageMobile: {
    width: "100vw",
    marginLeft: 0,
    height: "100vh",
  },
  border: {
    borderBottom: (props: any) =>
      (props.darkMode || !props.mobileSidebarHidden) &&
      `1px solid ${palette.divider}`,
    boxShadow: (props: any) =>
      props.darkMode
        ? "none"
        : !props.mobileSidebarHidden
        ? "0px 1.73333px 25.1333px rgba(0, 0, 0, 0.0103512)"
        : "0px 2px 5px rgba(0, 0, 0, 0.0746353)",
  },
  transitionItem: {
    opacity: 0,
    transform: "scale(0)",
    transformOrigin: "center 0",
    position: "fixed",
    transition:
      "opacity 354ms cubic-bezier(0.4, 0, 0.2, 1) 0ms, transform 236ms cubic-bezier(0.4, 0, 0.2, 1) 0ms",
  },
  transitionOpen: {
    opacity: 1,
    transform: "scale(1)",
  },
  gradBG: {
    background: "linear-gradient(90.17deg, #22329C 0.17%, #73D6F1 105.18%)",
  },
}));

export interface PageWithSidebarProps {
  children: any;
  hideAccountButtons?: boolean;
}

const PageWithSidebar: React.FC<PageWithSidebarProps> = ({
  children,
  hideAccountButtons = true,
}) => {
  const [tradingModalOpen, setTradingModalOpen] = useState(
    localStorage.getItem("tradingModalStatus") !== "closed"
  );
  const [mobileSidebarHidden, setMobileSidebarHidden] = useState(true);
  const theme = useTheme();
  const { palette } = theme;
  const [darkMode] = useDarkModeManager();
  const mobile = useMediaQuery(theme.breakpoints.down("sm"));
  const classes = useStyles({ darkMode, mobileSidebarHidden });

  const hideMobileMenu = () => {
    setMobileSidebarHidden(true);
  };

  useEffect(() => {
    if (!mobile && !mobileSidebarHidden) {
      setMobileSidebarHidden(true);
    }
  }, [mobile, mobileSidebarHidden]);

  if (
    localStorage.getItem("tradingModalStatus") === "closed" &&
    tradingModalOpen
  ) {
    setTradingModalOpen(false);
  }

  return (
    <Box bgcolor="background.default">
      <Grid container>
        {!mobile && (
          <Box
            display={"flex"}
            justifyContent={"space-between"}
            width={"100%"}
            alignItems={"center"}
            zIndex={"100"}
            position={"fixed"}
            style={{ backgroundColor: "white" }}
          >
            <Box my={"-10px"} ml={"-10px"}>
              <img
                height={"100px"}
                src={Ardana_hor_logo}
                alt="Ardana-hor-color"
              />
            </Box>
            <Box mr={"20px"}>
              <Button variant="contained">Connect Wallet</Button>
            </Box>
          </Box>
        )}
      </Grid>
      <Grid container>
        {!mobile && (
          <Box mt={"80px"} position="fixed" left={0} width={210}>
            <Sidebar />
          </Box>
        )}

        <Box className={cx(classes.page, mobile && classes.pageMobile)}>
          <Box
            position="fixed"
            width={mobile ? 1 : "calc(100vw - 210px)"}
            zIndex={10}
            bgcolor={!mobile ? "transparent" : palette.background.paper}
            pt={mobile ? "36px" : 3}
            // pr={mobile ? '50px' : 0}
            // pl={mobile ? '50px' : 0}
            px={mobile ? "9%" : 3}
            className={cx(mobile && classes.border && classes.gradBG)}
            height={mobile ? "152px" : "72px"}
          >
            <Box width="100%" display="flex" justifyContent="center">
              <Box
                display="flex"
                width="100%"
                maxWidth="1280px"
                justifyContent={!mobile ? "flex-end" : "space-between"}
                alignItems="center"
                pr={!mobile ? 3 : 0}
              >
                {mobile && (
                  <Box
                    display="flex"
                    alignItems="center"
                    justifyContent="space-between"
                    width="100%"
                  >
                    <Grid style={{ height: "48px", marginRight: "6px" }}>
                      <IconButton
                        style={{ height: "48px", padding: 0 }}
                        onClick={() =>
                          setMobileSidebarHidden(!mobileSidebarHidden)
                        }
                      >
                        <Hamburger
                          size={38}
                          distance={"lg"}
                          color={theme.palette.common.white}
                          toggled={!mobileSidebarHidden}
                          toggle={setMobileSidebarHidden}
                        />
                      </IconButton>
                    </Grid>
                    <Grid
                      container
                      component={Link}
                      to="/"
                      style={{ justifyContent: "center" }}
                    >
                      <Box marginTop="2px">
                        <img src={MainLogo} alt="main logo" width={"68px"} />
                      </Box>
                    </Grid>
                    <Box marginTop="2px">
                      <img src={Avatar} alt={Avatar} width={"40px"} />
                    </Box>
                  </Box>
                )}
                {/* {!mobile && (
                  <Box
                    display="flex"
                    justifySelf="flex-end"
                    style={{ backgroundColor: 'transparent' }}
                  >
                    <AccountButtons />
                  </Box>
                )} */}
              </Box>
            </Box>
          </Box>

          {mobile && (
            <Box
              position="fixed"
              zIndex={20}
              p={"20px"}
              top={"152px"}
              display="flex"
              justifyContent="flex-end"
              width="100vw"
              style={{ backgroundColor: "white" }}
            >
              <Button variant="contained">Connect Wallet</Button>
            </Box>
          )}

          {mobile && (
            <Box
              className={cx(
                classes.transitionItem,
                !mobileSidebarHidden && classes.transitionOpen
              )}
              width={mobile ? 1 : "calc(100vw - 230px)"}
              position="relative"
              mt="152px"
              mb={mobile ? 0 : 7}
              maxHeight="calc(100vh - 160px)"
              overflow="auto"
              style={{
                background: mobile
                  ? "linear-gradient(90.17deg, #22329C 0.17%, #73D6F1 105.18%)"
                  : "unset",
                backgroundColor: palette.background.paper,
                zIndex: 1000,
                height: "unset",
              }}
            >
              {/* <Box p={!mobile ? 1 : 0}>
                <AccountButtons onHide={hideMobileMenu} mobile />
              </Box> */}
              <Divider />
              <Box p={1} pl={1.25} pr={0}>
                <Sidebar mobile onHide={hideMobileMenu} />
              </Box>
              <Divider />
              {/* <Box p={1.5}>
                <ThemeSwitch />
              </Box> */}
              <Box
                borderBottom={`1px solid ${palette.divider}`}
                boxShadow={
                  darkMode ? "" : "0px 2px 5px rgba(0, 0, 0, 0.0746353)"
                }
              />
            </Box>
          )}

          {
            <>
              <Box
                px={mobile ? 0 : 3}
                width={mobile ? "100vw" : "calc(100vw - 230px)"}
                mt={!mobile ? "85px" : "230px"}
                mb={mobile ? 0 : "23px"}
              >
                <Container style={!mobile ? {} : { padding: "0 20px" }}>
                  {children}
                </Container>
                {/* {mobile && (
                  <Box width='100%' zIndex={14}>
                    <Footer />
                  </Box>
                )} */}
              </Box>
              {/* {!mobile && (
                <Box
                  position='fixed'
                  width='calc(100vw - 230px)'
                  bottom={0}
                  zIndex={14}
                  bgcolor={palette.background.default}
                >
                  <Footer />
                </Box>
              )} */}
            </>
          }
        </Box>
      </Grid>
    </Box>
  );
};

export default PageWithSidebar;
