import React, { useState } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    cursor: "pointer",
    position: "relative",
    lineHeight: "0px",
    display: "inline-block",
    width: "fit-content",
    zIndex: 102,
  },
  children: {
    position: "absolute",
    top: "0px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    height: "100%",
  },
}))

interface GradientBoxProps {
  width?: number
  height?: number
  strokeWidth?: number
  clickable?: boolean
}

const GradientBox: React.FC<GradientBoxProps> = ({
  width = 200,
  height = 50,
  strokeWidth = 3,
  clickable = true,
  children,
}) => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const [hover, setHover] = useState(false)

  return (
    <Box position="relative" lineHeight={0} display="inline-block">
      <Box
        className={cx(classes.root)}
        onMouseEnter={() => setHover(true)}
        onMouseLeave={() => setHover(false)}
      >
        <svg width={width + strokeWidth * 2} height={height + strokeWidth * 2}>
          <defs>
            <linearGradient id="grad1" x1="0" y1="0" x2="1" y2="0">
              <stop offset="0" stopColor="#5F72FF" />
              <stop offset="1" stopColor="#73D6F1" />
            </linearGradient>
          </defs>
          <g fill="none">
            <rect
              x={strokeWidth}
              y={strokeWidth}
              width={width}
              height={height}
              rx={height / 2}
              stroke="url(#grad1)"
              strokeWidth={strokeWidth}
            />
          </g>
        </svg>
        <Box className={cx(classes.children)}>{children}</Box>
      </Box>
      <Box
        position="absolute"
        top={strokeWidth + "px"}
        left={strokeWidth + "px"}
        width={`calc(100% - ${strokeWidth * 2}px)`}
        height={`calc(100% - ${strokeWidth * 2}px)`}
        borderRadius="500px"
        border={`${strokeWidth} solid transparent`}
        boxShadow="0px 0px 14px 7px #2D3BA0"
        zIndex={101}
        style={clickable && hover ? { background: "#FFFFFF33" } : {}}
      />
    </Box>
  )
}

export default GradientBox
