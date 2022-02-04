import * as T from "io-ts/Type"

import * as Item from "./Item"

export * as Item from "./Item"

/*
 * @category model
 */
export type APYChart = Item.Type[]

export type Type = APYChart

export const codec = T.array(Item.codec)
