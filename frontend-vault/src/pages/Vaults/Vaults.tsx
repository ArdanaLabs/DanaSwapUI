import React from "react"
import { Box } from "@mui/material"
import { HeroSection } from "./sections"
import { AssetSection } from "../Landing/sections"

const Vaults: React.FC = () => {
  return (
    <Box>
      <HeroSection />
      <Box mt={"50px"} />
      <AssetSection />
    </Box>
  )
}

export default Vaults
