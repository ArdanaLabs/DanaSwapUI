import * as T from "io-ts/Type"

import * as Item from "./Item"

export * as Item from "./Item"

/*
 * @category model
 */

export type FeeVolumeChart = Item.Type[]

export type Type = FeeVolumeChart

export const codec = T.array(Item.codec)
