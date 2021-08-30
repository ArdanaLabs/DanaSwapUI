import React, { useState } from "react";
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Link,
  FormControl,
  MenuItem,
} from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";

import LOGO_Ardana_ver from "assets/img/landing/logos/ardana-ver.svg";
import { setLang } from "hooks";
import { Select } from "components";

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: `linear-gradient(90.19deg, #2F3DA0 48.37%, #73D6F1 99.87%)`,
    color: "#FFFFFF",
  },

  link: {
    color: "white",
    lineHeight: "25px",
    display: "block",
    fontFamily: "Futura",
  },

  socialIconLink: {
    borderRadius: "50%",
    backgroundColor: "white",
    padding: "10px",
    marginRight: "20px",
    cursor: "pointer",
    color: "gray",
    textAlign: "center",
    transition: "background .2s",
    fontFamily: "auto",

    "& i": {
      width: "16px",
      height: "16px",
    },

    "&:hover": {
      backgroundColor: "lightgray",
    },
  },

  ".MuiMenu-paper": {
    background: "white",
    color: "black",
  }
}));

const FooterSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  const [currentLang, setCurrentLang] = useState(
    window.localStorage.i18nextLng
  );

  const handleLangChange = (e: any) => {
    let newLang = e.target.value;
    setCurrentLang(newLang);
    setLang(newLang);
  };

  return (
    <Box className={cx(classes.bg)} p={!mobile ? "120px 0 100px" : "50px 0"}>
      <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
        <Container>
          <Grid container spacing={1}>
            <Grid item xs={12} sm={3}>
              <Box
                textAlign="center"
                flexDirection="column"
                display="flex"
                alignItems="center"
              >
                <img
                  src={LOGO_Ardana_ver}
                  alt="Ardana logo"
                  style={{ maxWidth: "max-content" }}
                />
                <FormControl>
                  <Select
                    labelId="demo-simple-select-label"
                    id="demo-simple-select"
                    value={currentLang}
                    onChange={handleLangChange}
                  >
                    <MenuItem value={"en"}>English</MenuItem>
                    <MenuItem value={"zh"}>Chinese</MenuItem>
                  </Select>
                </FormControl>
              </Box>
            </Grid>
            <Grid item xs={12} sm={9}>
              <Box
                border="1px solid white"
                width={!mobile ? "80%" : "100%"}
              ></Box>

              <Box mt={!mobile ? "50px" : "30px"}></Box>

              <Grid container spacing={3}>
                <Grid item xs={12} sm={6}>
                  <Box textAlign="left">
                    <Grid container>
                      <Grid item xs={6} sm={4}>
                        <Box
                          fontWeight="500"
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="23px"
                        >
                          {i18next.t("PAGE.LANDING.FOOTER.RESOURCES.LABEL")}
                        </Box>
                        <Box mt="20px"></Box>
                        <Box
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="21px"
                        >
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.RESOURCES.LIST.0")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.RESOURCES.LIST.1")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.RESOURCES.LIST.2")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.RESOURCES.LIST.3")}
                          </Link>
                        </Box>
                      </Grid>
                      <Grid item xs={6} sm={4}>
                        <Box
                          fontWeight="500"
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="23px"
                        >
                          {i18next.t("PAGE.LANDING.FOOTER.PRODUCTS.LABEL")}
                        </Box>
                        <Box mt="20px"></Box>
                        <Box
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="21px"
                        >
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.PRODUCTS.LIST.0")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.PRODUCTS.LIST.1")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.PRODUCTS.LIST.2")}
                          </Link>
                        </Box>
                      </Grid>
                      <Grid item xs={6} sm={4}>
                        <Box
                          fontWeight="500"
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="23px"
                        >
                          {i18next.t("PAGE.LANDING.FOOTER.FOUNDATION.LABEL")}
                        </Box>
                        <Box mt="20px"></Box>
                        <Box
                          fontSize={!mobile ? "18px" : "16px"}
                          lineHeight="21px"
                        >
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.FOUNDATION.LIST.0")}
                          </Link>
                          <Link href="#" className={cx(classes.link)}>
                            {i18next.t("PAGE.LANDING.FOOTER.FOUNDATION.LIST.1")}
                          </Link>
                        </Box>
                      </Grid>
                    </Grid>
                  </Box>
                </Grid>
                <Grid item xs={12} sm={6}>
                  <Box pl={!mobile ? "100px" : "0px"}>
                    <Box
                      fontWeight="500"
                      fontSize={!mobile ? "18px" : "16px"}
                      lineHeight="23px"
                    >
                      {i18next.t("PAGE.LANDING.FOOTER.SOCIAL.LABEL")}
                    </Box>
                    <Box mt="20px"></Box>
                    <Box className={cx(classes.link)}>
                      {i18next.t("PAGE.LANDING.FOOTER.SOCIAL.CONTENT")}
                    </Box>
                    <Box lineHeight="50px">
                      <Link className={cx(classes.socialIconLink)} href="#">
                        <i className="fab fa-twitter"></i>
                      </Link>
                      <Link className={cx(classes.socialIconLink)} href="#">
                        <i className="fab fa-instagram"></i>
                      </Link>
                      <Link className={cx(classes.socialIconLink)} href="#">
                        <i className="fab fa-medium"></i>
                      </Link>
                      <Link className={cx(classes.socialIconLink)} href="#">
                        <i className="fab fa-youtube"></i>
                      </Link>
                      <Link className={cx(classes.socialIconLink)} href="#">
                        <i className="fab fa-linkedin"></i>
                      </Link>
                    </Box>
                  </Box>
                </Grid>
              </Grid>
            </Grid>
          </Grid>
        </Container>
      </ScrollAnimation>
    </Box>
  );
};

export default FooterSection;
