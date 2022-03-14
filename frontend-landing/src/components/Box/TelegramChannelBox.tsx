import React from "react"
import { Box, Link, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import { GradientBox } from "."

import NextIcon from "assets/icons/carousel-next.svg"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: palette.background.paper,
    borderRadius: "10px",
    padding: "20px",
  },
}))

interface TelegramChannelBoxProps {
  country: string
  flag: string
  link: string
}

const TelegramChannelBox: React.FC<TelegramChannelBoxProps> = ({
  country,
  flag,
  link,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <Box
        display={"flex"}
        justifyContent={"space-between"}
        alignItems="center"
      >
        <img src={flag} alt={country} />
        <Link href={link} target="_blank">
          <GradientBox width={35} height={35}>
            <img src={NextIcon} alt="next" width={"10px"} />
          </GradientBox>
        </Link>
      </Box>
      <Box mb="10px" />
      <Typography component="h5" variant="h5">
        {`${country} Telegram`}
      </Typography>
    </Box>
  )
}

export default TelegramChannelBox
