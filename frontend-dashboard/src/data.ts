import LOGO_Ardana from "assets/coins/ardana.png"
import LOGO_Cardano from "assets/coins/cardano.png"

import { Currency } from "pages/Swap/Swap"

export const FontFamilies = {
  Brandon: "Brandon Grotesque, fantasy",
  Museo: "Museo Sans, sans-serif",
}

export const navList = [
  {
    label: "Home",
    link: "/home",
  },
  {
    label: "Pools",
    link: "/pools",
  },
  {
    label: "Swap",
    link: "/swap",
  },
  {
    label: "Use Dana",
    link: "/dana",
  },
  {
    label: "Dashboard",
    link: "/dashboard",
  },
  {
    label: "Arem",
    link: "/arem",
  },
  {
    label: "Info",
    link: "/info",
  },
]

export const options = [
  {
    title: "Filter",
    data: [
      { label: "Insert", value: "Insert" },
      { label: "Filter", value: "Filter" },
      { label: "Names", value: "Names" },
      { label: "Here", value: "Here" },
      { label: "Accordingly", value: "Accordingly" },
    ],
  },
  {
    title: "Slippage",
    data: [
      { label: "0.5%", value: "0.5" },
      { label: "1%", value: "1" },
      { label: "", value: "Custom", hasInput: true },
    ],
  },
  {
    title: "Gas Prices",
    data: [
      { label: "20.5 Slow", value: "20.5" },
      { label: "25 Standard", value: "25" },
      { label: "28 Fast", value: "28" },
      { label: "32 Instant", value: "32" },
      { label: "", value: "Custom", hasInput: true },
    ],
  },
]

export const Currencies: Currency[] = [
  {
    imageUrl: LOGO_Ardana,
    denom: "DANA",
    minimalDenom: "exDANA",
    decimals: 6,
  },
  {
    imageUrl: LOGO_Cardano,
    denom: "ADA",
    minimalDenom: "exADA",
    decimals: 6,
  },
]

export const PoolRatePerDANAList = [
  {
    pool: "compound",
    rate: 0.793,
  },
  {
    pool: "compound",
    rate: 0.793,
  },
  {
    pool: "compound",
    rate: 0.793,
  },
  {
    pool: "compound",
    rate: 0.793,
  },
  {
    pool: "compound",
    rate: 0.793,
  },
  {
    pool: "compound",
    rate: 0.793,
  },
]

export const WeeklyFeeList = [
  {
    week: "Thu Jul 08 2021 (in progress)",
    fee: "0",
  },
  {
    week: "Thu Jul 1 2021",
    fee: "149,000,100",
  },
  {
    week: "Thu Jul 1 2021",
    fee: "149,000,100",
  },
  {
    week: "Thu Jul 1 2021",
    fee: "149,000,100",
  },
  {
    week: "Thu Jul 1 2021",
    fee: "149,000,100",
  },
  {
    week: "Thu Jul 1 2021",
    fee: "149,000,100",
  },
]

export const depositePools = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
