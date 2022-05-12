import React, {useEffect} from "react"
import { Box } from "@material-ui/core"

import { ProfileSection, AdvisorsSection, MainSection } from "./sections"

const Team: React.FC = () => {
  useEffect(() => {
    window.scrollTo(0, 0)
  }, [])
  return (
    <Box>
      <MainSection />
      <ProfileSection />
      <AdvisorsSection />
    </Box>
  )
}

export default Team
