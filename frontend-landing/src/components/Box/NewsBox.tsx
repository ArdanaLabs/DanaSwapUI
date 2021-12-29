import React from "react"
import cx from "classnames"
import { Box, Link, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import { GradientBox } from "components/Box"

export interface NewsBoxProps {
  image: string
  title: string
  content: string
  date: string
  link: string
}

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    marginTop: "30px",
    marginBottom: "30px",
  },
  body: {
    padding: "20px 15px 0px",
    borderLeft: "1px solid #72D2F3AA",
  },
  date: {
    color: palette.secondary.main,
  },
  content: {
    display: "-webkit-box",
    maxWidth: "100%",
    [`-webkit-line-clamp`]: 3,
    [`-webkit-box-orient`]: "vertical",
    [`overflow`]: "hidden",
  },
}))

const NewsBox: React.FC<NewsBoxProps> = ({
  image,
  title,
  content,
  date,
  link,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Box lineHeight={0}>
        <img src={image} alt="news" width={"100%"} />
      </Box>
      <Box className={classes.body}>
        <Typography component="h5" variant="h5">
          {title}
        </Typography>
        <Box mb="15px" />
        <Typography component="h4" variant="h4" className={classes.date}>
          {date}
        </Typography>
        <Box mb="20px" />
        <Typography component="h4" variant="h4" className={classes.content}>
          {content}
        </Typography>
        <Box mb="20px" />
        <Link href={link} target="_blank">
          <GradientBox width={140} height={40}>
            <Typography component="div" variant="button">
              Read More
            </Typography>
          </GradientBox>
        </Link>
      </Box>
    </Box>
  )
}

export default NewsBox
