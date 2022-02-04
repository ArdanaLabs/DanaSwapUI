import { createAction } from "@reduxjs/toolkit"

import { CombinedStats } from "Data/Stats/CombinedStats"
import { FetchDecodeResult } from "Data/FetchDecode"

export const receivedCombinedStats = createAction<
  FetchDecodeResult<CombinedStats>
>("home/receivedCombinedStats")
