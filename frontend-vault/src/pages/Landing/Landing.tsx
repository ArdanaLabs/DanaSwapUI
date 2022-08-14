import React from "react"
import { Box, useMediaQuery, useTheme } from "@mui/material"
import {
  AdSection,
  AssetSection,
  HelpSection,
  FeatureSection,
} from "./sections"

const Landing: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))

  return (
    <Box display="flex" flexDirection="column" gap={!mobile ? "100px" : "70px"}>
      <AdSection />

      <FeatureSection />

      <AssetSection />

      <HelpSection />
    </Box>
  )
}

export default Landing
