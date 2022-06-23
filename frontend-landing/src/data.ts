import { ProfileType } from "components/Box/ProfileBox"
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"
import { ReactComponent as DiscordIcon } from "assets/icons/discord.svg"
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg"
import { ReactComponent as MediumIcon } from "assets/icons/medium.svg"
import { ReactComponent as YoutubeIcon } from "assets/icons/youtube.svg"
import { ReactComponent as LinkedinIcon } from "assets/icons/linkedin.svg"

export interface NavInfoType {
  label: string
  url: URL
}

export const Menus: NavInfoType[] = [
  {
    label: "Home",
    url: new URL("/", document.baseURI),
  },
  {
    label: "Technology",
    url: new URL("/tech", document.baseURI),
  },
  {
    label: "Documentation",
    url: new URL("https://docs.ardana.org/"),
  },
  {
    label: "Community",
    url: new URL("/community", document.baseURI),
  },
  {
    label: "News",
    url: new URL("/news", document.baseURI),
  },
  // {
  //   label: "Roadmap",
  //   // to: "https://faceted-wash-97d.notion.site/cb0d147034e6439f8e70b2698ce199f2?v=fbf6185ab5f143eb9e22064fd9647814",
  //   url: new URL("/roadmap", document.baseURI),
  // },
]

export const ArdanaFeatures = [
  {
    image: require("assets/logos/fully-decentralized.png").default,
    title: "Fully\nDecentralized",
    content: "Unbiased, collateral backed and pegged to the US Dollar.",
  },
  {
    image: require("assets/logos/borrowing.png").default,
    title: "Borrowing",
    content: "Allows users to borrow stablecoins against locked collateral.",
  },
  {
    image: require("assets/logos/store-of-value.png").default,
    title: "Store of\nValue",
    content: "Secure store of value preserving value even in volatile markets.",
  },
  {
    image: require("assets/logos/powered-by-cardano.png").default,
    title: "Powered by\nCardano",
    content: "Built utilizing Cardano’s speed, scalability and security.",
  },
]

export const DanaSwapFeatures = [
  {
    image: require("assets/logos/ultra-low-slippage.png").default,
    title: "Ultra-low Slippage",
    content:
      "Swap between stablecoins and stable assets such as wrapped/synthetic Bitcoin with minimal slippage.",
  },
  {
    image: require("assets/logos/earn-trading-fees.png").default,
    title: "Earn Trading Fees",
    content:
      "Deposit your assets into a DanaSwap pool and earn a proportion of the market making fees.",
  },
  {
    image: require("assets/logos/foreign-exchange.png").default,
    title: "Foreign Exchange",
    content:
      "Swap between international stablecoins such as dUSD, dEUR, dGBP and more.",
  },
  {
    image: require("assets/logos/dana-token.png").default,
    title: "DANA Token",
    content:
      "The governance token rewarded to users for supporting the ecosystem through liquidity provision.",
  },
  {
    image: require("assets/logos/governance.png").default,
    title: "Governance",
    content:
      "DANA token holders can participate in polling and voting to influence the development of Ardana.",
  },
]

export const Members: ProfileType[] = [
  {
    avatar: require("assets/avatars/RyanMatovu.png").default,
    name: "Ryan Matovu",
    role: "Founder",
    info: `Serial entrepreneur in B2B/B2C sales and e-commerce.\n\nLeadership positions in various Ethereum based projects.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/ryan-matovu-517988203",
      // twitter: "https://twitter.com/ryanmatovu",
      twitter:
        "https://twitter.com/infinit3booker?t=QBsb3CsDPsTNStkDIdlLgA&s=09",
    },
  },
  {
    avatar: require("assets/avatars/IssacShapira.png").default,
    name: "Isaac Shapira",
    role: "Chief Technology Officer",
    info: `Founder of Platonic.Systems, a lead functional programming firm.\n\nSenior software developer roles at various leading tech firms.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/isaacshapira/",
      github: "https://github.com/Fresheyeball",
    },
  },
  {
    avatar: require("assets/avatars/DerekEvans.png").default,
    name: "Derek Evans",
    role: "Chief Operating Officer",
    info: `20+ years in software leading business and government projects.\n\nPrivate trader and fund manager in the financial derivatives market.`,
    socials: {},
  },
  {
    avatar: require("assets/avatars/BassamAwad.png").default,
    name: "Bassam Awad, PhD",
    role: "Chief Economist",
    info: `Senior Economist and Head of Risk at the <strong>Central Bank of Jordan.</strong>\n\nMultiple peer-reviewed papers on macro/computational economics.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/bassamrawad/",
    },
  },
  {
    avatar: require("assets/avatars/ScottFranklin.png").default,
    name: "Scott Franklin",
    role: "Head of Strategy",
    info: `20+ years experience in equity markets on Wall Street.\n\nCapital raising and advisor to multiple blockchain projects.`,
    socials: {},
  },
  {
    avatar: require("assets/avatars/NeilTionson.png").default,
    name: "Neil Tionson",
    role: "Head of Design",
    info: `Marketing coordinator for Five Group Pty Ltd.\n\nWorked with brands such as Cadbury, San Remo.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/neil-tiongson-a83535142/",
      // github: "https://github.com",
    },
  },
  {
    avatar: require("assets/avatars/ChetanBafna.png").default,
    name: "Chetan Bafna",
    role: "Head of Outreach",
    info: `Fundraising facilitator to multiple blockchain projects.\n\nLeadership positions in multiple philanthropic organisations.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/chetanbafna",
    },
  },
  // {
  //   avatar: require("assets/avatars/MorganThomas.png").default,
  //   name: "Morgan Thomas",
  //   role: "Blockchain Developer",
  //   info: `Platonic Systems functional programming tech consultant.\n\nDeveloped algorithmic trading systems using Haskell.`,
  //   socials: {
  //     linkedin: "https://www.linkedin.com/in/morgan-thomas-29a923b7/",
  //     github: "https://github.com/morganthomas",
  //   },
  // },
  {
    avatar: require("assets/avatars/NicVanDenBroeck.png").default,
    name: "Nick Van den Broeck",
    role: "Blockchain Developer",
    info: `Masters degree in Theoretical Physics and research intern at <strong>CERN</strong>.\n\nFounder of Tailer AI, bringing AI to the chemical/brewing industries.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/nick-van-den-broeck-860861a3/",
      github: "https://github.com/CSVdB/",
    },
  },
  {
    avatar: require("assets/avatars/GregorySantini.png").default,
    name: "Gregory Santini",
    role: "Front-end Developer",
    info: `Senior front-end developer for <strong>Apple</strong> and Evernote.\n\nExperienced web developer and Solidity developer.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/gregory-santini-28a51619a/",
      github: "https://github.com/elite0226",
    },
  },
  // {
  //   avatar: require("assets/avatars/MarcinBugaj.png").default,
  //   name: "Marcin Bugaj",
  //   role: "Blockchain Developer",
  //   info: `Senior software developer for <strong>Logitech</strong> and Motorola Solutions.\n\nDesign and implementation of Robotic Process Automation (RPA).`,
  //   socials: {
  //     linkedin: "https://pl.linkedin.com/in/mmbugaj/",
  //     github: "https://github.com/Josek-dev",
  //   },
  // },
  {
    avatar: require("assets/avatars/JaimeCasoOnzain.png").default,
    name: "Jamie Caso Onzain",
    role: "Blockchain Developer",
    info: `<strong>Cardano</strong> ambassador and Cardano stake pool operator.\n\nGenesis Founding Member and tester for <strong>Mina Protocol.</strong>`,
    socials: {
      linkedin: "https://www.linkedin.com/in/jaime-caso/",
      github: "https://github.com/jimcase/",
    },
  },
  {
    avatar: require("assets/avatars/AndrzejSwatowski.png").default,
    name: "Andrzej Swatowski",
    role: "Blockchain Developer",
    info: `Game developer in Unity working on AI,UI and gameplay elements.\n\nMasters Degree in Computer Science from the University of Warsaw.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/andrzejswatowski",
      github: "https://github.com/swtwsk/",
    },
  },
  {
    avatar: require("assets/avatars/OlegPrutz.png").default,
    name: "Oleg Prutz",
    role: "Blockchain Developer",
    info: `Data Scientist for Genesys AI ‘Experience as a Service’ company.\n\nExperienced functional programmer and Kaggle competitor.`,
    socials: {
      // linkedin: "https://www.linkedin.com/",
      github: "https://github.com/prutz1311",
    },
  },
  {
    avatar: require("assets/avatars/EmmaDwyer.png").default,
    name: "Emma Dwyer",
    role: "Writer",
    info: `Risk analyst at <strong>Citi bank</strong> and AML analyst at <strong>State Street.</strong>\n\n7+ years experience in investment banking and wealth management.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/emma-dwyer-169630a0/",
      // github: "https://github.com/prutz1311",
    },
  },
  {
    avatar: require("assets/avatars/NoamDavidElbaz.png").default,
    name: "Noam David Elbaz",
    role: "Researcher",
    info: `Crypto portfolio management and DeFi enthusiast.`,
    socials: {},
  },
  {
    avatar: require("assets/avatars/EdenOvadia.png").default,
    name: "Eden Ovadia",
    role: "Intern",
    info: `Full stack software engineer for <strong>Microsoft.</strong>\n\nSoftware and programming, problems solving skills and attention to details.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/eden-ben-ovadia-944053176/",
      // github: "https://github.com/",
    },
  },
  {
    avatar: require("assets/avatars/GregNwosu.png").default,
    name: "Greg Nwosu",
    role: "Intern",
    info: `Big Data and infrastructure engineer for <strong>Barclays</strong> and RBS.\n\nSenior Java developer for <strong>IG Index</strong> online exchange.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/greg-nwosu/",
      github: "https://github.com/gregnwosu",
    },
  },
]

export const Advisors: ProfileType[] = [
  {
    avatar: require("assets/avatars/JohnOConnor.png").default,
    name: "John O'Connor",
    role: "Cardano",
    info: `Director of <strong>IOHK</strong> leading operations and projects in Africa.\n\nFormer Head of Strategy/Comms for <strong>Cardano Foundation.</strong>`,
    socials: {
      linkedin: "https://www.linkedin.com/in/jjtoconnor/",
    },
  },
  {
    avatar: require("assets/avatars/ShunsukeMurasaki.png").default,
    name: "Shunsuke Murasaki",
    role: "Emurgo",
    info: `Chief executive officer for <strong>EMURGO</strong> Indonesia.\n\n7+ years experience in Asian markets in B2B overseas sales.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/shunsuke-murasaki-942012ba/",
    },
  },
  {
    avatar: require("assets/avatars/DarrenCamas.png").default,
    name: "Darren Camas",
    role: "Cardano",
    info: `Former senior advisor to <strong>Emurgo</strong> and strategy consultant for <strong>IOHK.</strong>\n\nFounder of IPOR Labs, blockchain based derivatives software.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/dcamas/",
    },
  },
  {
    avatar: require("assets/avatars/PedroBatista.png").default,
    name: "Pedro Batista",
    role: "CBDC",
    info: `<strong>UK Bank of England</strong> review for Central Bank Digital Currencies.\n\nGlobal Head of Payments/Banking for various financial institutions.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/joaopedrobatista/",
    },
  },
  {
    avatar: require("assets/avatars/JoeriVanGeelen.png").default,
    name: "Joeri Van Geelen",
    role: "Economics",
    info: `Business advisor at <strong>Prysm Group</strong> DLT economic consulting firm.\n\nBusiness Development advisor at Minterest cross-chain protocol.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/joerivangeelen/",
    },
  },
  // {
  //   avatar: require("assets/avatars/YosefShaftal.png"),
  //   name: "Yosef Shaftal",
  //   role: "Business",
  //   info: `Executive director of the <strong>Israeli Blockchain Association.</strong>\n\nStrategic advisor to startups, corporations and governments.`,
  //   socials: {
  //     linkedin: "https://www.linkedin.com",
  //   },
  // },
  // {
  //   avatar: require("assets/avatars/JDGagnon.png"),
  //   name: "JD Gagnon",
  //   role: "DeFi",
  //   info: `Founder of <strong>Benqi</strong>, liquidity market protocol on Avalanche.\nDivision director at IG Wealth Management.`,
  //   socials: {
  //     linkedin: "https://www.linkedin.com",
  //   },
  // },
  {
    avatar: require("assets/avatars/CalEvans.png").default,
    name: "Cal Evans",
    role: "Legal",
    info: `Cryptocurrency compliance expert with 50+ ICO/STO contributions.\n\n<strong>British Blockchain Association</strong> board member.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/mrcalevans/",
    },
  },
  {
    avatar: require("assets/avatars/JoaoSantos.png").default,
    name: "Joao Santos",
    role: "Venture Capital",
    info: `Senior Principal at Mustard Seed Maze venture capital fund.\n\nMentor at the Catolica Lisbon School of Business and Economics.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/joao-ferrao-dos-santos/",
    },
  },
]

export const TopNotchTeams = [
  require("assets/logos/apple.svg").default,
  require("assets/logos/microsoft.svg").default,
  require("assets/logos/unity.svg").default,
  require("assets/logos/barclays.svg").default,
  require("assets/logos/statestreet.svg").default,
  require("assets/logos/cardano.svg").default,
  require("assets/logos/emurgo.svg").default,
  require("assets/logos/mina.svg").default,
]

export const Partners = [
  {
    src: require("assets/logos/platonic-systems.svg").default,
    url: "https://platonic.systems/",
  },
  {
    src: require("assets/logos/occamfi.svg").default,
    url: "https://occam.fi/",
  },
  { src: require("assets/logos/coti.svg").default, url: "https://coti.io/" },
  {
    src: require("assets/logos/elrond.svg").default,
    url: "https://elrond.com/",
  },
  {
    src: require("assets/logos/mlabs.svg").default,
    url: "https://mlabs.city/",
  },
  {
    src: require("assets/logos/liqwid.svg").default,
    url: "https://www.liqwid.finance/",
  },
  {
    src: require("assets/logos/near.svg").default,
    url: "https://near.org/",
  },
  {
    src: require("assets/logos/chaoslabs.svg").default,
    url: "https://chaoslabs.xyz/",
  },
  {
    src: require("assets/logos/indigo.png").default,
    url: "https://indigoprotocol.io/",
  },
  // require("assets/logos/mlabs.svg").default,
  // require("assets/logos/defire.svg").default,
  // require("assets/logos/israel-bc.svg").default,
  // require("assets/logos/halborn.svg").default,
  // require("assets/logos/prysm.svg").default,
  // require("assets/logos/synaps.svg").default,
  // require("assets/logos/singularity.svg").default,
  // require("assets/logos/benqi.svg").default,
]

export const Investors = [
  {
    src: require("assets/logos/three-arrow-capital.svg").default,
    url: "https://www.threearrowscap.com/select-investments/",
  },
  { src: require("assets/logos/cfund.svg").default, url: "https://cfund.vc/" },
  {
    src: require("assets/logos/ascentive-assets.svg").default,
    url: "https://ascensiveassets.com/",
  },
  {
    src: require("assets/logos/kronos.svg").default,
    url: "https://kronosresearch.com/",
  },
  {
    src: require("assets/logos/morning-star.svg").default,
    url: "https://www.morningstar.ventures/",
  },
  {
    src: require("assets/logos/mechanism-capital.svg").default,
    url: "https://www.mechanism.capital/",
  },
  {
    src: require("assets/logos/QCP-capital.svg").default,
    url: "https://qcp.capital/",
  },
  {
    src: require("assets/logos/angels.svg").default,
    url: "https://www.iangels.com/",
  },
  { src: require("assets/logos/david-post.svg").default, url: "" },
  { src: require("assets/logos/taiyang-zhang.svg").default, url: "" },
  { src: require("assets/logos/justin-sun.svg").default, url: "" },
  // require("assets/logos/cryptodorum.svg").default,
  // require("assets/logos/nodeseeds.svg").default,
  // require("assets/logos/tenzor-capital.svg").default,
  // require("assets/logos/au21-capital.svg").default,
  // require("assets/logos/prometeus.svg").default,
  // require("assets/logos/faculty-group.svg").default,
  // require("assets/logos/defi-capital.svg").default,
  // require("assets/logos/emerging-star-capital.svg").default,
  // require("assets/logos/deltahub.svg").default,
  // require("assets/logos/alpha-sigma-capital.svg").default,
  // require("assets/logos/redacted.svg").default,
  // require("assets/logos/double-peak.svg").default,
  // require("assets/logos/trgc.svg").default,
  // require("assets/logos/kosmos.svg").default,
  // require("assets/logos/fabric-ventures.svg").default,
  // require("assets/logos/skynet-trading.svg").default,
  // require("assets/logos/bitscale-capital.svg").default,
  // require("assets/logos/arrington-capital.svg").default,
  // require("assets/logos/autonomy-capital.svg").default,
  // require("assets/logos/richard-ma.svg").default,
  // require("assets/logos/two-sigma-ventures.svg").default,
  // require("assets/logos/cardwallet.svg").default,
  // require("assets/logos/indigo.svg").default,
  // require("assets/logos/sundaeswap.svg").default,
]

export const externals: {
  [group: string]: { [subgroup: string]: URL | null }
} = {
  Resources: {
    "Technology": new URL("/tech", document.baseURI),
    "Documentation": new URL("https://docs.ardana.org/"),
    // "Pitch Deck": "https://docsend.com/view/kc5m2snw7t77fs5r",
    "Pitch Deck": new URL("https://docsend.com/view/ps9hsr88m834pj98"),
    "Brand Assets": new URL("/brandassets", document.baseURI),
  },
  Products: {
    Dashboard: null,
    DEX: null,
    Stablecoin: null,
  },
  Company: {
    "Team": new URL("/team", document.baseURI),
    "Contact Us": new URL("mailto:enquiries@ardana.org"),
    "Careers": new URL("mailto:careers@ardana.org"),
  },
}

export const socials = [
  {
    image: TwitterIcon,
    url: "https://twitter.com/ardanaproject",
  },
  {
    image: DiscordIcon,
    url: "https://discord.gg/c9skrZvsqH",
  },
  {
    image: TelegramIcon,
    url: "https://t.me/ardanaofficial",
  },
  {
    image: MediumIcon,
    url: "https://medium.com/ardana-hub",
  },
  {
    image: YoutubeIcon,
    url: "https://www.youtube.com/channel/UCuVtpKzlmsD6s0ZiC0hakkA",
  },
  {
    image: LinkedinIcon,
    url: "https://www.linkedin.com/company/ardanalabs/",
  },
]

export const TechnicalPapers = [
  {
    title: "eUTXO Models",
    link: "https://docsend.com/view/d2g9qajrb4w23eyq",
    image: require("assets/papers/eUTXOModels.png").default,
  },
  {
    title: "Danaswap\nTechnical Paper",
    link: "https://docsend.com/view/v4w3muusi6im3ay2",
    image: require("assets/papers/DanaswapTechnicalPaper.png").default,
  },
]

export const Listings = [
  {
    image: require("assets/listings/gate.io.svg").default,
    link: "https://www.gate.io/trade/DANA_USDT",
  },
  // {
  //   image: require("assets/listings/BitMart.svg").default,
  //   link: "https://www.bitmart.com/trade/en?symbol=DANA_USDT&layout=basic",
  // },
  {
    image: require("assets/listings/MEXC.svg").default,
    link: "https://www.mexc.com/exchange/DANA_USDT",
  },
]

export const Roadmap = {
  "Q1 2021": ["Project Conception", "Danaswap Development"],
  "Q2 2021": ["Stablecoin Development", "Seed Sale", "Whitepaper Release"],
  "Q3 2021": ["Plutus Contract Audits", "Private Sale", "AREM Development"],
  "Q4 2021": ["Public Sale", "Token Generation Event", "CEX Listing"],
  "Q1 2022": ["Futher Wallet Integration", "Ardana Academy Opening"],
  "Q2 2022": ["Community Ambassador Program", "Governance Transition"],
  "Q3 2022": [
    "Third Party Integration Expansion",
    "Partnership Project Integrations",
    "Stablecoin Release",
    "Danaswap Release",
  ],
  "Q4 2022": ["Danaswap Foreign Exchange", "Danaswap V2 Release"],
  "2023+": [
    "Tokenized Asset Vaults",
    "Alternative Stablecoin Currency Vaults",
    "Mobile Application",
    "Real World Asset Vaults",
    "Fiat Gateways",
    "Institutional Platform Development",
    "Liquidity Incentives Program",
  ],
}

export const CommunityList = [
  {
    image: require("assets/logos/newspaper.png").default,
    title: "Find out the\nlatest news",
    content:
      "Be up to the date on the latest Ardana updates  and announcements.",
    cta: {
      label: "Ardana Hub on Medium",
      link: "https://medium.com/ardana-hub",
      width: 220,
      height: 40,
    },
  },
  {
    image: require("assets/logos/communication-chat-bubble.png").default,
    title: "Join the\nconversation",
    content: "Join our Telegram channel and Discord server.",
    cta: {
      label: "Follow us on Twitter",
      link: "https://twitter.com/ardanaproject",
      width: 220,
      height: 40,
    },
  },
  {
    image: require("assets/logos/startup.png").default,
    title: "Become an\nambassador",
    content: "Be part of the Team and become an Ardana ambassador.",
    cta: {
      label: "Coming Soon",
      link: "/",
      width: 160,
      height: 40,
    },
  },
]

export const TelegramChannelList = [
  {
    country: "Chinese",
    flag: require("assets/telegramchannels/flags/chinese.svg").default,
    link: "/",
  },
  {
    country: "German",
    flag: require("assets/telegramchannels/flags/german.svg").default,
    link: "/",
  },
  {
    country: "Indonesian",
    flag: require("assets/telegramchannels/flags/indonesian.svg").default,
    link: "/",
  },
  {
    country: "Italian",
    flag: require("assets/telegramchannels/flags/italian.svg").default,
    link: "/",
  },
  {
    country: "Japanese",
    flag: require("assets/telegramchannels/flags/japanese.svg").default,
    link: "/",
  },
  {
    country: "Korean",
    flag: require("assets/telegramchannels/flags/southkorea.svg").default,
    link: "/",
  },
  {
    country: "Portuguese",
    flag: require("assets/telegramchannels/flags/portuguese.svg").default,
    link: "/",
  },
  {
    country: "Russian",
    flag: require("assets/telegramchannels/flags/russian.svg").default,
    link: "/",
  },
  {
    country: "Spanish",
    flag: require("assets/telegramchannels/flags/spanish.svg").default,
    link: "/",
  },
  {
    country: "Thai",
    flag: require("assets/telegramchannels/flags/thai.svg").default,
    link: "/",
  },
  {
    country: "Turkish",
    flag: require("assets/telegramchannels/flags/turkish.svg").default,
    link: "/",
  },
  {
    country: "Vietnamese",
    flag: require("assets/telegramchannels/flags/vietnamese.svg").default,
    link: "/",
  },
]

export const NewsOnMediumList = [
  {
    image: require("assets/news/medium/ASPA_PARTNER_FOCUS_8.png").default,
    title:
      "ASPA Partner Focus #8 — Announcing the Latest Stake Pool Operators Partnering with Ardana",
    content:
      "Welcome to the eighth edition of our ASPA Partner Focus series. We have interviewed the latest new joiners and would like to share their mission and Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum",
    date: "December 16, 2021",
    link: "https://medium.com/ardana-hub/aspa-partner-focus-8-announcing-the-latest-stake-pool-operators-partnering-with-ardana-1f4f8f3ca45a?source=collection_home---4------1-----------------------",
  },
  {
    image: require("assets/news/medium/ASPA_PARTNER_FOCUS_7.png").default,
    title:
      "ASPA Partner Focus #7 — Announcing the Latest Stake Pool Operators Partnering with Ardana",
    content:
      "In the seventh edition of our ASPA Partner Focus series, we welcome the latest ASPA joiners to Ardana! ASPA applications are now closed! We sat down and Lorem Lorem Lorem ",
    date: "December 9, 2021",
    link: "https://medium.com/ardana-hub/aspa-partner-focus-7-announcing-the-latest-stake-pool-operators-partnering-with-ardana-23bff202860f?source=collection_home---4------2-----------------------",
  },
  {
    image: require("assets/news/medium/ASPA_PARTNER_FOCUS_6.png").default,
    title:
      "ASPA Partner Focus #6 — Announcing the Latest Stake Pool Operators Partnering with Ardana",
    content:
      "Please join us in welcoming the latest ASPA joiners to Ardana! We sat down and interviewed each stake pool to find out their mission and goals, who they are as Lorem Lorem Lorem Lorem Lorem Lorem",
    date: "December 5, 2021",
    link: "https://medium.com/ardana-hub/aspa-partner-focus-6-announcing-the-latest-stake-pool-operators-partnering-with-ardana-ae67525924c4?source=collection_home---4------3-----------------------",
  },
  {
    image: require("assets/news/medium/NEAR_PROTOCOL.png").default,
    title: "Ardana is Partnering with NEAR Protocol",
    content:
      "Today, we at Ardana are excited to announce our latest strategic partnership with NEAR, a decentralized application platform (dApp) that focuses on both developer and user experience. Through this partnership with NEAR, which includes a grant",
    date: "November 23, 2021",
    link: "https://medium.com/ardana-hub/ardana-is-partnering-with-near-protocol-281a6e64e031?source=collection_home---4------4-----------------------",
  },
  {
    image: require("assets/news/medium/DANA_TOKEN.png").default,
    title: "DANA Token Going Live on Gate, Bitmart and MEXC!",
    content:
      "The growing community around Ardana has been eagerly awaiting the token launch since the project began. Today, we are thrilled to announce that the token launch will take place on Monday, 22nd...",
    date: "November 17, 2021",
    link: "https://medium.com/ardana-hub/dana-token-going-live-on-gate-bitmart-and-mxec-72ed54702527?source=collection_home---4------5-----------------------",
  },
  {
    image: require("assets/news/medium/ARDANA_QCP.png").default,
    title:
      "Crypto Options leader QCP Capital Makes a Strategic Investment in Ardana",
    content:
      "Today, we are very pleased to announce that QCP Capital has made a strategic investment in Ardana. We are excited to be supported by QCP Capital, who has a strong presence in the Asian markets as we",
    date: "December 5, 2021",
    link: "https://medium.com/ardana-hub/crypto-options-leader-qcp-capital-makes-a-strategic-investment-in-ardana-e51de7397e35?source=collection_home---4------6-----------------------",
  },
]

export const NewsOnTwitterList = [
  {
    title: "Ardana - DeFi Hub of Cardano",
    type: "@ArdanaProject",
    content: `⭐️ Ardana has formed a strategic partnership w/ <span>#dApp</span> platform <span>@NEARProtocol</span>\n\n🌈 Through this collab, which includes a grant, we'll <span>#BUIDL</span> the first-ever bridge between <span>#Cardano</span> & <span>#NEAR</span> ecosystems to facilitate seamless asset transfers and more ⤵️`,
    image: require("assets/news/twitter/twitter1.png").default,
    datetime: "10:21 PM · Nov 23, 2021",
  },
  {
    title: "Ardana - DeFi Hub of Cardano",
    type: "@ArdanaProject",
    content: `4/ ✨ WIthout this protection measure, collateral could be manipulated, for example withdrawn from a vault under false pretences. The operations of Emergency Oracles and price delays by the OGM are also managed by <span>#Ardana</span> governance.`,
    datetime: "10:21 PM · Nov 23, 2021",
  },
  {
    title: "Ardana - DeFi Hub of Cardano",
    type: "@ArdanaProject",
    content: `3/ 🌐 The OGM serves as a layer of defense between the oracles and the protocol by delaying the price feed by one hour. This delay allows Emergency Oracles or an #Ardana governance vote to individually freeze compromised oracles.`,
    datetime: "10:21 PM · Nov 23, 2021",
  },
]

export const NewsOnMediaList = [
  {
    image: require("assets/news/media/Y4SEOFO3SNHCZMJOIWYGPOZD24 1.png")
      .default,
    label: "Coindesk",
    title: "Ardana and Near Join Forces to Build a Crosschain Bridge",
    content:
      "On Tuesday morning, Ardana, a stablecoin and lending hub on the Cardano blockchain platform, announced an agreement with Near protocol, a layer 1 Ethereum alternative. Ardana received a grant",
    link: "https://www.coindesk.com/business/2021/11/23/ardana-and-near-join-forces-to-build-a-crosschain-bridge/#:~:text=Ardana%20received%20a%20grant%20from,press%20release%20provided%20to%20CoinDesk.&text=In%20October%2C%20Near%20announced%20an,a%20bid%20to%20attract%20developers",
  },
  {
    label: "CoinTelegraph",
    image:
      require("assets/news/media/1434_aHR0cHM6Ly9zMy5jb2ludGVsZWdyYXBoLmNvbS91cGxvYWRzLzIwMjEtMTEvNzZhNjcyYjYtZGQ0NS00NjViLThlNTUtNTEwYzViNmJkZjA4LmpwZw== 1.png")
        .default,
    title:
      "Ardana partners with Near protocol on Cardano bridge infrastructure",
    content:
      "Ardana, Cardano’s growing stablecoin hub, announced a strategic partnership with Near protocol on Tuesday. The partnership allows for asset transfer between the two protocols in which Ardana will",
    link: "https://cointelegraph.com/news/ardana-partners-with-near-protocol-on-cardano-bridge-infrastructure",
  },
  {
    image: require("assets/news/media/69ea73c64e0929176878276fd614c409 1.png")
      .default,
    label: "Yahoo Finance",
    title: "Cardano DeFi platform Ardana partners with NEAR protocol",
    content:
      "Recently-launched Ardana – a Cardano based decentralised stablecoin solution – has partnered with Ethereum rival NEAR protocol.The move aims to deliver an interconnected bridge between the DeFi",
    link: "https://finance.yahoo.com/news/cardano-defi-platform-ardana-partners-135929378.html",
  },
  {
    image:
      require("assets/news/media/cardano-stablecoin-protocol-ardana-toasts-10m-capital-injection-led-by-three-arrows-capital-and-ascensive-assets 1.png")
        .default,
    label: "Bitcoin.com",
    title:
      "Cardano Stablecoin Protocol Ardana Toasts $10m Capital Injection Led by Three Arrows Capital and Ascensive Assets",
    content:
      "Ardana, the asset-backed stablecoin protocol and decentralized exchange, has attracted $10 million of investment in its latest strategic funding round",
    link: "https://news.bitcoin.com/cardano-stablecoin-protocol-ardana-toasts-10m-capital-injection-led-by-three-arrows-capital-and-ascensive-assets/",
  },
  {
    label: "CoinDesk",
    image: require("assets/news/media/KHAH5Z2K3FBRHDQTPX4UVPVK3Q 1.png")
      .default,
    title: "Three Arrows Capital Backs $10M Raise for DeFi on Cardano",
    content:
      "Cardano’s decentralized finance (DeFi) ecosystem may be showing glimmers of primordial life following the close of a $10 million raise for Ardana, a new protocol that aims to provide stablecoin minting",
    link: "https://www.coindesk.com/tech/2021/10/29/three-arrows-capital-backs-10m-raise-for-defi-on-cardano/",
  },
  {
    image:
      require("assets/news/media/cardano-stablecoin-protocol-ardana-toasts-10m-capital-injection-led-by-three-arrows-capital-and-ascensive-assets 1.png")
        .default,
    label: "Benzinga",
    title:
      "Cardano Stablecoin Protocol Ardana Toasts $10m Capital Injection Led by Three Arrows Capital and Ascensive Assets",
    content:
      "Ardana, the asset-backed stablecoin protocol and decentralized exchange, has attracted $10 million of investment in its latest strategic funding round",
    link: "https://www.benzinga.com/markets/cryptocurrency/21/10/23749212/cardano-stablecoin-protocol-ardana-closes-10m-round-led-by-three-arrows-capital-ascensive-",
  },
]
