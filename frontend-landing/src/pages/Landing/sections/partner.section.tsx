import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Link,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import { Partners } from "data"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    // background: palette.background.default,
    padding: "50px 20px",

    [breakpoints.down("xs")]: {
      padding: "30px 0",
    },
  },
  title: {
    color: palette.secondary.main,
    lineHeight: "100%",
    textAlign: "center",
  },
  partner: {
    "textAlign": "center",

    "& img": {
      width: "100%",
      maxWidth: "max-content",
    },
  },
}))

const PartnerSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.bg)}>
      <Container maxWidth="md">
        <Typography variant="h3" component="h3" className={cx(classes.title)}>
          Partners
        </Typography>

        <Box
          display="flex"
          flexWrap="wrap"
          alignItems="center"
          justifyContent="center"
          mt="30px"
          style={{ opacity: 0.8 }}
        >
          {Partners.map((partner, index) => (
            <Box
              key={index}
              textAlign="center"
              p={!mobile ? "20px 0px" : "10px"}
              flex="1 0 21%"
            >
              {partner.url && (
                <Link href={partner.url} target="_blank" underline="none">
                  <img
                    src={partner.src}
                    alt="partner"
                    height={!mobile ? "40px" : "25px"}
                    style={{ maxWidth: "max-content" }}
                  />
                </Link>
              )}
              {!partner.url && (
                <img
                  src={partner.src}
                  alt="partner"
                  height={!mobile ? "40x" : "25px"}
                  style={{ maxWidth: "max-content" }}
                />
              )}
            </Box>
          ))}
          {/* {Partners.flatMap((partner, index) => [
              <Box key={index} textAlign="center" p={!mobile ? "20px" : "10px"}>
                {partner.url && (
                  <Link href={partner.url} target="_blank" underline="none">
                    <img
                      src={partner.src}
                      alt="partner"
                      height={!mobile ? "45px" : "25px"}
                      style={{ maxWidth: "max-content" }}
                    />
                  </Link>
                )}
                {!partner.url && (
                  <img
                    src={partner.src}
                    alt="partner"
                    height={!mobile ? "45px" : "25px"}
                    style={{ maxWidth: "max-content" }}
                  />
                )}
              </Box>,
              (index + 1) % 4 === 0 && !mobile && (
                <Box key={"wrap" + index} flexBasis="100%" />
              ),
            ])} */}
        </Box>
      </Container>
    </Box>
  )
}

export default PartnerSection
