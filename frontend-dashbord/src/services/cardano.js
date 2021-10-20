import CardanoWeb3 from 'publish/cardano-web3-browser'

export const config = process.env.REACT_APP_NETWORK === 'mainnet'
  ? {
      network: 'mainnet',
      graphql: 'https://api-mainnet-graphql.raynet.work',
    }
  : {
      network: 'testnet',
      graphql: 'https://api-testnet-graphql.raynet.work',
    }


const Cardano = new CardanoWeb3({
  crypto: {
    network: config.network,
  },
  explorer: {
    url: config.graphql,
    responseHandler: (response) => {
      const { data } = response
      if (data.errors) {
        data.errors.forEach(() => {
          console.log({
            message: 'Something went wrong :(',
            description: 'Please try to update your wallet data or reload the app',
          })
        })
        return false
      }
      return response
    },
    errorHandler: () => {
      console.log({
        message: 'Something went wrong :(',
      })
    },
  },
})

// Cardano.addProvider('helper', function Helper(pkg) {
//   this.settings = pkg.settings
// })

export default Cardano
