
import LOGO_Ardana from "assets/logos/ardana.png";
import LOGO_Cardano from "assets/logos/cardano.png";

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
    ],
  },
];

export const navList = [
  {
    label: "HOME",
    link: "/home",
  },
  {
    label: "POOLS",
    link: "/pools",
  },
  {
    label: "SWAP",
    link: "/swap",
  },
  {
    label: "USE DANA",
    link: "/dana",
  },
  {
    label: "ANALYTICS",
    link: "/analytics",
  },
  {
    label: "INFO",
    link: "/info",
  },
];

export const TokenList = [
  {
    src: LOGO_Ardana,
    name: "DANA",
    desc: "exDANA",
  },
  {
    src: LOGO_Cardano,
    name: "ADA",
    desc: "exDANA",
  },
];

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
];

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
];

export const depositePools = [
  "A",
  "B",
  "C",
  "D",
  "E",
  "F",
  "G",
  "H",
  "I",
];