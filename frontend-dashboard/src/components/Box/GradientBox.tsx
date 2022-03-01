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
  glow?: boolean
  onClick: () => void
}

const GradientBox: React.FC<GradientBoxProps> = ({
  width = 200,
  height = 50,
  strokeWidth = 3,
  clickable = true,
  glow = true,
  children,
  onClick,
}) => {
  const { breakpoints, palette } = useTheme()
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
        onClick={onClick}
      >
        <svg width={width + strokeWidth} height={height + strokeWidth}>
          <defs>
            <linearGradient id="grad1" x1="0" y1="0" x2="1" y2="0">
              <stop offset="0" stopColor={palette.secondary.dark} />
              <stop offset="1" stopColor={palette.secondary.main} />
            </linearGradient>
          </defs>
          <g fill="none">
            <rect
              x={strokeWidth / 2}
              y={strokeWidth / 2}
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
        boxShadow={glow ? "0px 0px 14px 7px #2D3BA0" : "none"}
        zIndex={101}
        style={clickable && hover ? { background: "#FFFFFF33" } : {}}
      />
    </Box>
  )
}

export default GradientBox
