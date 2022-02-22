import LOGO_Ardana from "assets/coins/ardana.png"
import LOGO_Cardano from "assets/coins/cardano.png"

import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"
import { ReactComponent as DiscordIcon } from "assets/icons/discord.svg"
import { ReactComponent as LinkedinIcon } from "assets/icons/linkedin.svg"
import { ReactComponent as MediumIcon } from "assets/icons/medium.svg"
import { ReactComponent as YoutubeIcon } from "assets/icons/youtube.svg"
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg"

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

export const Socials = [
  {
    name: "Twitter",
    icon: TwitterIcon,
    link: "https://twitter.com/ardanaproject",
  },
  {
    name: "Discord",
    icon: DiscordIcon,
    link: "https://discord.gg/c9skrZvsqH",
  },
  {
    name: "Telegram",
    icon: TelegramIcon,
    link: "https://t.me/ardanaofficial",
  },
  {
    name: "Medium",
    icon: MediumIcon,
    link: "https://medium.com/ardana-hub",
  },
  {
    name: "Youtube",
    icon: YoutubeIcon,
    link: "https://www.youtube.com/channel/UCuVtpKzlmsD6s0ZiC0hakkA",
  },
  {
    name: "Linkedin",
    icon: LinkedinIcon,
    link: "https://www.linkedin.com/company/ardanalabs/",
  },
]
