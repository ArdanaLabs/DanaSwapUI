import Coin1 from 'assets/coin1.png'
import Coin2 from 'assets/coin2.png'
import Coin3 from 'assets/coin3.png'

export const options = [
  {
    title: 'Filter',
    data: [
      { label: 'Insert', value: 'Insert' },
      { label: 'Filter', value: 'Filter' },
      { label: 'Names', value: 'Names' },
      { label: 'Here', value: 'Here' },
      { label: 'Accordingly', value: 'Accordingly' }
    ]
  },
  {
    title: 'Max Slippage',
    data: [
      { label: '0.5%', value: '0.5' },
      { label: '1%', value: '1' },
      { label: 'Custom:', value: 'Custom', hasInput: true }
    ]
  },
  {
    title: 'Gas Prices',
    data: [
      { label: '20.5 Slow', value: '20.5' },
      { label: '25 Standard', value: '25' },
      { label: '28 Fast', value: '28' },
      { label: '32 Instant', value: '32' }
    ]
  }
]

export const pools = {
  columns: [
    { name: 'Pool', col: 4 },
    { name: 'Base APY', col: 3 },
    { name: 'Rewards APY', col: 3 },
    { name: 'Volumn', col: 2, sortable: true }
  ],
  records: [
    {
      pool: {
        icon: Coin1,
        currency: 'sUSD',
        description: 'DAI + USDC + USDT + sUSD'
      },
      baseAPY: '2.99%',
      rewardsAPY: '+4.30% -> 10.76% CRV+1.13% SNX',
      volumn: '$106.5m'
    },
    {
      pool: {
        icon: Coin2,
        currency: 'sUSD',
        description: 'DAI + USDC + USDT + sUSD'
      },
      baseAPY: '2.99%',
      rewardsAPY: '+4.30% -> 10.76% CRV+1.13% SNX',
      volumn: '$106.5m'
    },
    {
      pool: {
        icon: Coin3,
        currency: 'sUSD',
        description: 'DAI + USDC + USDT + sUSD'
      },
      baseAPY: '2.99%',
      rewardsAPY: '+4.30% -> 10.76% CRV+1.13% SNX',
      volumn: '$106.5m'
    }
  ]
}
