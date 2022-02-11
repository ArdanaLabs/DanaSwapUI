import * as IO from "io-ts"
import { getLenses, optionFromNullable } from "io-ts-types"
import { readonlyMapFromRecord } from "io-ts-utils"

import * as Address from "Data/Address"
import * as AmplificationCoefficient from "Data/AmplificationCoefficient"
import * as ByTxType from "Data/ByTxType"
import * as LiquidityTokenPrice from "Data/LiquidityTokenPrice"
// import { typeNAV } from "Data/NAV"
import { Percent, USD } from "Data/Unit"

import * as AssetBalances from "./AssetBalances"

export * as AssetBalances from "./AssetBalances"

// TODO: should these be wrapped in Newtypes like the back-end???

export const codec = IO.type(
  {
    recentDailyAPYPercent: optionFromNullable(Percent.codec),
    recentDailyVolumeUSD: ByTxType.codec(optionFromNullable(USD.codec)),
    recentWeeklyAPYPercent: optionFromNullable(Percent.codec),
    recentMonthlyAPYPercent: optionFromNullable(Percent.codec),
    recentAnnualAPYPercent: optionFromNullable(Percent.codec),
    totalAPYPercent: optionFromNullable(Percent.codec),
    dailyFeeVolumeUSD: optionFromNullable(USD.codec),
    amplificationCoefficient: optionFromNullable(
      AmplificationCoefficient.codec
    ),
    feePercent: optionFromNullable(Percent.codec),
    adminFeePercent: optionFromNullable(Percent.codec),
    liquidityUtilization: optionFromNullable(IO.number),
    virtualPriceUSD: optionFromNullable(
      //readonlyMapFromRecord(SubPoolAddress.codec, VirtualPrice.codec)
      readonlyMapFromRecord(Address.codec, USD.codec)
    ),
    navUSD: optionFromNullable(USD.codec), // NAV.codec
    liquidityTokenPriceUSD: optionFromNullable(LiquidityTokenPrice.codec),
    dailyTxCount: optionFromNullable(ByTxType.codec(IO.number)),
    reserves: AssetBalances.codec,
  },
  "PoolStats"
)

export type PoolStats = IO.TypeOf<typeof codec>

export type Type = PoolStats

export const lenses = getLenses(codec)
