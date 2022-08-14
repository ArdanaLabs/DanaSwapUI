import React from "react"
import { Box } from "@mui/material"
import { HeroSection } from "./sections"
import { AssetSection } from "../Landing/sections"

const Vaults: React.FC = () => {
  return (
    <Box display={"flex"} flexDirection={"column"} gap={"50px 0px"}>
      <HeroSection />
      <AssetSection />
    </Box>
  )
}

export default Vaults
