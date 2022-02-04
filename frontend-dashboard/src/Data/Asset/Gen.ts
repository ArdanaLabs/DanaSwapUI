import { Arbitrary, string } from "fast-check"

import * as Asset from "Data/Asset"

export const genAsset: Arbitrary<Asset.Type> = string().map(Asset.iso.wrap)
