import i18next from "i18next";
import { ProfileType } from "components/Box/ProfileBox";
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg";
import { ReactComponent as DiscordIcon } from "assets/icons/discord.svg";
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg";
import { ReactComponent as MediumIcon } from "assets/icons/medium.svg";
import { ReactComponent as YoutubeIcon } from "assets/icons/youtube.svg";
import { ReactComponent as LinkedinIcon } from "assets/icons/linkedin.svg";

export const Menus = [
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.0"),
    to: "/",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.1"),
    to: "/tech",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.2"),
    to: "https://docs.ardana.org/",
    blank: true,
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.3"),
    // to: "https://faceted-wash-97d.notion.site/cb0d147034e6439f8e70b2698ce199f2?v=fbf6185ab5f143eb9e22064fd9647814",
    to: "/roadmap",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.4"),
    to: "/team",
  },
  {
    label: i18next.t("PAGE.LANDING.HEADER.LINKS.5"),
    to: "https://medium.com/ardana-hub",
    blank: true,
  },
];

export const ArdanaFeatures = [
  {
    image: require("assets/logos/fully-decentralized.png").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.CONTENT"),
  },
  {
    image: require("assets/logos/borrowing.png").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.CONTENT"),
  },
  {
    image: require("assets/logos/store-of-value.png").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.CONTENT"),
  },
  {
    image: require("assets/logos/powered-by-cardano.png").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.CONTENT"),
  },
];

export const DanaSwapFeatures = [
  {
    image: require("assets/logos/ultra-low-slippage.png").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.0.CONTENT"),
  },
  {
    image: require("assets/logos/earn-trading-fees.png").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.1.CONTENT"),
  },
  {
    image: require("assets/logos/foreign-exchange.png").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.2.CONTENT"),
  },
  {
    image: require("assets/logos/dana-token.png").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.3.CONTENT"),
  },
  {
    image: require("assets/logos/governance.png").default,
    title: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.4.TITLE"),
    content: i18next.t("PAGE.LANDING.DANASWAP.FEATURES.4.CONTENT"),
  },
];

export const Members: ProfileType[] = [
  {
    avatar: require("assets/avatars/RyanMatovu.png").default,
    name: "Ryan Matovu",
    role: "Founder",
    info: `Serial entrepreneur in B2B/B2C sales and e-commerce.\n\nLeadership positions in various Ethereum based projects.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/ryan-matovu-517988203",
      twitter: "https://twitter.com/ryanmatovu",
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
  {
    avatar: require("assets/avatars/MorganThomas.png").default,
    name: "Morgan Thomas",
    role: "Blockchain Developer",
    info: `Platonic Systems functional programming tech consultant.\n\nDeveloped algorithmic trading systems using Haskell.`,
    socials: {
      linkedin: "https://www.linkedin.com/in/morgan-thomas-29a923b7/",
      github: "https://github.com/morganthomas",
    },
  },
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
  {
    avatar: require("assets/avatars/MarcinBugaj.png").default,
    name: "Marcin Bugaj",
    role: "Blockchain Developer",
    info: `Senior software developer for <strong>Logitech</strong> and Motorola Solutions.\n\nDesign and implementation of Robotic Process Automation (RPA).`,
    socials: {
      linkedin: "https://pl.linkedin.com/in/mmbugaj/",
      github: "https://github.com/Josek-dev",
    },
  },
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
];

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
  //   avatar: require("assets/avatars/YosefShaftal.png").default,
  //   name: "Yosef Shaftal",
  //   role: "Business",
  //   info: `Executive director of the <strong>Israeli Blockchain Association.</strong>\n\nStrategic advisor to startups, corporations and governments.`,
  //   socials: {
  //     linkedin: "https://www.linkedin.com",
  //   },
  // },
  // {
  //   avatar: require("assets/avatars/JDGagnon.png").default,
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
];

export const TopNotchTeams = [
  require("assets/logos/apple.svg").default,
  require("assets/logos/microsoft.svg").default,
  require("assets/logos/unity.svg").default,
  require("assets/logos/barclays.svg").default,
  require("assets/logos/statestreet.svg").default,
  require("assets/logos/cardano.svg").default,
  require("assets/logos/emurgo.svg").default,
  require("assets/logos/mina.svg").default,
];

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
  // require("assets/logos/mlabs.svg").default,
  // require("assets/logos/defire.svg").default,
  // require("assets/logos/israel-bc.svg").default,
  // require("assets/logos/halborn.svg").default,
  // require("assets/logos/prysm.svg").default,
  // require("assets/logos/synaps.svg").default,
  // require("assets/logos/singularity.svg").default,
  // require("assets/logos/benqi.svg").default,
];

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
];

export const externals: any = {
  Resources: {
    Technology: "/tech",
    Documentation: "https://docs.ardana.org/",
    // "Pitch Deck": "https://docsend.com/view/kc5m2snw7t77fs5r",
    "Pitch Deck": "https://docsend.com/view/ps9hsr88m834pj98",
    "Brand Assets": "/brandassets",
  },
  Products: {
    Dashboard: "#",
    DEX: "#",
    Stablecoin: "#",
  },
  Company: {
    Team: "/team",
    "Contact Us": "mailto:enquiries@ardana.org",
    Careers: "mailto:careers@ardana.org",
  },
};

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
];

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
];

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
];

export const Roadmap = {
  "Q1 2021": ["Project Conception", "Danaswap Development"],
  "Q2 2021": ["Stablecoin Development", "Seed Sale", "Whitepaper Release"],
  "Q3 2021": ["Plutus Contract Audits", "Private Sale", "AREM Development"],
  "Q4 2021": [
    "Public Sale",
    "Token Generation Event",
    "CEX Listing",
    "Ardana Academy Opening",
  ],
  "Q1 2022": [
    "Futher Wallet Integration",
    "Stablecoin Release",
    "Danaswap Release",
  ],
  "Q2 2022": [
    "Danaswap V2 Release",
    "Community Ambassador Program",
    "Governance Transition",
  ],
  "Q3 2022": [
    "Third Party Integration Expansion",
    "Partnership Project Integrations",
  ],
  "Q4 2022": ["Danaswap Foreign Exchange"],
  "2023+": [
    "Tokenized Asset Vaults",
    "Alternative Stablecoin Currency Vaults",
    "Mobile Application",
    "Real World Asset Vaults",
    "Fiat Gateways",
    "Institutional Platform Development",
    "Liquidity Incentives Program",
  ],
};
