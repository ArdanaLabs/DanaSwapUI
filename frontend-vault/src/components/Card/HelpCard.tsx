import React from "react"
import { Box, Typography, useTheme, Theme } from "@mui/material"
import { makeStyles } from "@mui/styles"
import ArrowRightIcon from "assets/image/svgs/arrow-right-circle.svg"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    position: "relative",
    borderRadius: "30px",
    padding: "50px 40px",
    color: theme.palette.common.white,
    display: "flex",
    boxShadow: "10px 10px 30px rgba(0, 0, 0, 0.05)",
    cursor: "pointer",
    marginBottom: "30px",

    [theme.breakpoints.down("sm")]: {
      padding: "20px 50px 100px 50px",
      textAlign: "center",
    },
  },
  title: {
    marginBottom: "20px",
  },
  content: {
    whiteSpace: "pre-line",

    [theme.breakpoints.down("sm")]: {
      whiteSpace: "unset",
    },
  },

  link: {
    position: "absolute",
    top: "50%",
    right: "0px",
    transform: "translate(-50%, -50%)",

    [`& > img`]: {
      width: "50px",

      [theme.breakpoints.down("sm")]: {
        width: "40px",
        transform: "rotateZ(90deg)",
      },
    },

    [theme.breakpoints.down("sm")]: {
      top: "unset",
      bottom: "0px",
      left: "50%",
    },
  },
}))

export interface HelpCardProps {
  title: string
  content: string
  background: string
}

const HelpCard: React.FC<HelpCardProps> = ({ title, content, background }) => {
  const theme = useTheme()
  const classes = useStyles(theme)

  return (
    <Box className={classes.root} style={{ background: background }}>
      <Box display="flex" flexDirection="column">
        <Typography variant="h1" className={classes.title}>
          {title}
        </Typography>
        <Typography variant="h4" className={classes.content}>
          {content}
        </Typography>
      </Box>
      <Box className={classes.link}>
        <img src={ArrowRightIcon} alt="" />
      </Box>
    </Box>
  )
}

export default HelpCard
