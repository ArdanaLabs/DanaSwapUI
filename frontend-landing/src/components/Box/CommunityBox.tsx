import React from "react"
import cx from "classnames"
import { Box, Link, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import { GradientBox } from "components/Box"

export interface CommunityBoxProps {
  image: string
  title: string
  content: string
  cta: {
    label: string
    link: string
    width: number
    height: number
  }
  short?: boolean
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    padding: "80px 30px 80px 30px",
    marginTop: "110px",
    marginBottom: "30px",
    textAlign: "center",
    background: palette.background.paper,
    borderRadius: "10px",
  },

  title: {
    whiteSpace: "pre-line",
  },

  image: {
    position: "absolute",
    top: "-10px",
    left: "50%",
    transform: "translate(-50%, -50%)",
  },

  cta: {
    position: "absolute",
    bottom: "0px",
    left: "50%",
    transform: "translate(-50%, -50%)",
  },
}))

const CommunityBox: React.FC<CommunityBoxProps> = ({
  image,
  title,
  content,
  cta,
  short = false,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.image)}>
        <GradientBox width={150} height={150}>
          <img src={image} alt="" />
        </GradientBox>
      </Box>
      <Typography component="h5" variant="h5" className={cx(classes.title)}>
        {title}
      </Typography>
      <Box mb={"20px"} />
      {!short && (
        <Typography component="h4" variant="h4">
          {content}
        </Typography>
      )}
      <Box mb={"30px"} />
      <Link href={cta.link} target={"_blank"} className={classes.cta}>
        <GradientBox width={cta.width} height={cta.height}>
          <Typography component="div" variant="button">
            {cta.label}
          </Typography>
        </GradientBox>
      </Link>
    </Box>
  )
}

export default CommunityBox
