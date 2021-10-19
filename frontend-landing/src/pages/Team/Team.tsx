import React from "react";
import { Box } from "@material-ui/core";

import {
  ProfileSection,
  AdvisorsSection,
} from "./sections";

const Team: React.FC = () => {
  return (
    <Box>
      <ProfileSection />
      <AdvisorsSection />
    </Box>
  );
};

export default Team;
