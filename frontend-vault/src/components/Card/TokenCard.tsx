import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    borderRadius: "30px",
    padding: "20px",
    color: "#FFFFFF",
    position: "relative",
    textAlign: "left",
    marginBottom: "35px",
  },
  typographyPrimary: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "normal",
  },
  label: {
    fontSize: "18px",
    lineHeight: "115%",
    marginTop: "5px",

    [breakpoints.down("xs")]: {
      fontSize: "14px",
    },
  },
  name: {
    fontSize: "40px",
    lineHeight: "110%",
    marginTop: "25px",
    marginBottom: "55px",

    [breakpoints.down("xs")]: {
      fontSize: "30px",
      marginBottom: "35px",
    },
  },
  status: {
    fontSize: "14px",
    lineHeight: "115%",
    opacity: "0.8",

    [breakpoints.down("xs")]: {
      "whiteSpace": "pre-line",

      "& > div": {
        flexBasis: "50%",
      },
    },
  },
  image: {
    position: "absolute",
    right: "0px",
    top: "-50px",

    [breakpoints.down("xs")]: {
      "& img": {
        width: "160px",
      },
    },
  },
}))

export interface TokenCardProps {
  image: string
  label: string
  name: string
  stabilityFee: number
  ratio: number
  background: string
}

const TokenCard: React.FC<TokenCardProps> = ({
  image,
  label,
  name,
  stabilityFee,
  ratio,
  background,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)} style={{ background: background }}>
      <Box className={cx(classes.image)}>
        <img src={image} alt="token" />
      </Box>

      <Box
        className={cx(classes.label, classes.typographyPrimary)}
        component={"p"}
      >
        {label}
      </Box>

      <Box
        className={cx(classes.name, classes.typographyPrimary)}
        component={"p"}
      >
        {name}
      </Box>

      <Box
        display="flex"
        justifyContent="space-between"
        className={cx(classes.typographySecondary, classes.status)}
      >
        <Box>{`Stability Fee \n${stabilityFee.toFixed(2)}%`}</Box>
        <Box>{`Min Coll. Ratio \n${ratio.toFixed(0)}%`}</Box>
      </Box>
    </Box>
  )
}

export default TokenCard
