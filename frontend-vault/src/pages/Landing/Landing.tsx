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
    <Box>
      <AdSection />

      <Box mt={!mobile ? "100px" : "70px"} />

      <FeatureSection />

      <Box mt={"30px"} />

      <AssetSection />

      <Box mt={!mobile ? "150px" : "100px"} />

      <HelpSection />
    </Box>
  )
}

export default Landing
