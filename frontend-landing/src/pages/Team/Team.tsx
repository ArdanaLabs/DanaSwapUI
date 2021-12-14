import React from "react"
import { Box } from "@material-ui/core"

import { ProfileSection, AdvisorsSection, MainSection } from "./sections"

const Team: React.FC = () => {
  return (
    <Box>
      <MainSection />
      <ProfileSection />
      <AdvisorsSection />
    </Box>
  )
}

export default Team
