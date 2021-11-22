import React from "react";
import { Box, useMediaQuery, Container, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import { useHistory } from "react-router-dom";

import { useIsDarkMode } from "state/user/hooks";

import DUSD_LOGO_BLUE from 'assets/image/DUSD-LOGO-BLUE.png'
import DUSD_LOGO_WHITE from 'assets/image/DUSD-LOGO-WHITE.png'

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: palette.background.default,
    marginTop: "50px",
    padding: 10,
    filter: 'drop-shadow(35px 0px 15px rgba(0, 0, 0, 0.12))',
  },

  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    "& img": {
      padding: '20px 10px',
      width: '65px',
      
      [breakpoints.down('sm')]: {
        width: '50px',
      }
    },
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
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
      fontSize: '15px',
    },

    "&:hover": {
      backgroundColor: "lightgray",
    },

    [breakpoints.down('sm')]: {
      padding: '6px 8px',
      marginRight: '15px',
      
      "& i": {
        fontSize: '12px',
      },
    }
  },
}));

const Footer: React.FC = () => {
  const theme = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(theme.breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });
  const history = useHistory();

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)} onClick={() => history.push('/')}>
            <img
              src={
                theme.palette.type === 'dark'
                  ? DUSD_LOGO_WHITE
                  : DUSD_LOGO_BLUE
              }
              alt='DANA Logo'
            />
          </Box>
          <Box>
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
      </Container>
    </Box>
  );
};

export default Footer;
