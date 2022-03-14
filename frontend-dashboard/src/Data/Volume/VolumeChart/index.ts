import * as IO from "io-ts"

import * as Item from "./Item"

export * as Item from "./Item"

/*
 * @category model
 */

// TODO: the back-end has this as a Map

export type VolumeChart = Item.Type[]

export type Type = VolumeChart

export const codec = IO.array(Item.codec)
