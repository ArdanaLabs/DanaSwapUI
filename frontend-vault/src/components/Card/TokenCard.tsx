import React from "react"
import cx from "classnames"
import { Box, Theme, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    borderRadius: "30px",
    padding: "20px",
    color: "#FFFFFF",
    position: "relative",
    textAlign: "left",
    marginBottom: "35px",
  },
  typographyPrimary: {
    fontFamily: FontFamilies.Brandon,
    fontStyle: "normal",
    fontWeight: 900,
  },
  typographySecondary: {
    fontFamily: FontFamilies.Museo,
    fontStyle: "normal",
    fontWeight: "normal",
  },
  label: {
    fontSize: "18px",
    lineHeight: "115%",
    marginTop: "5px",
    textTransform: "uppercase",

    [theme.breakpoints.down("sm")]: {
      fontSize: "14px",
    },
  },
  name: {
    fontSize: "40px",
    lineHeight: "110%",
    marginTop: "25px",
    marginBottom: "55px",
    textTransform: "uppercase",

    [theme.breakpoints.down("sm")]: {
      fontSize: "30px",
      marginBottom: "35px",
    },
  },
  status: {
    fontSize: "14px",
    lineHeight: "115%",
    opacity: "0.8",

    [theme.breakpoints.down("sm")]: {
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

    [theme.breakpoints.down("sm")]: {
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
  const theme = useTheme()
  const classes = useStyles(theme)

  return (
    <Box className={classes.root} style={{ background: background }}>
      <Box className={classes.image}>
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
