<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Yield Farming](#yield-farming)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [Yeild Farming](#yield-farming)
    - [Yield Farming Implementation](#yield-farming-implementation)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [Testing](#testing)
  - [Case study](#case-study)

<!-- markdown-toc end -->

# Yield Farming

## Introduction

Yield farming, also referred to as liquidity mining, is a way to generate rewards with cryptocurrency holdings. In simple terms, it means locking up cryptocurrencies and getting rewards.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-developer-ecosystem-the-evolution/anastasia-labs-open-source-production-grade-dapps).

## Documentation

### Yeild Farming

Yield farming is a key concept in the decentralized finance (DeFi) space. It involves providing liquidity to a DeFi protocol, typically through a pair of crypto assets, in exchange for rewards. These rewards often come in the form of additional digital tokens or interest payments. Here's a brief introduction to the concept:

#### Basic concept

- **Decentralized Finance (DeFi)**: Yield farming is a practice within the DeFi ecosystem, which aims to recreate traditional financial systems (like banks and exchanges) in a decentralized manner using blockchain technology.

- **Liquidity Provision**: Users, often called liquidity providers (LPs), add their crypto assets to liquidity pools. These pools power a marketplace where users can lend, borrow, or exchange tokens. The use of these pools can vary from simple token swaps to more complex financial activities like borrowing and lending.

- **Earning Rewards**: In return for providing liquidity, users earn rewards. These rewards can be in the form of transaction fees generated from the underlying DeFi platform or additional tokens issued by the protocol. The return is often denominated as an annual percentage yield (APY) or annual percentage rate (APR).

#### How Does Yield Farming Work?

- **Smart Contracts**: Yield farming uses smart contracts, which are self-executing contracts with the terms of the agreement directly written into code. These smart contracts manage the liquidity pools and the distribution of rewards.

- **Liquidity Pools**: A liquidity pool is a collection of funds locked in a smart contract. Users contribute tokens to the pool.

- **Staking and Pooling**: After depositing their tokens into a pool, LPs receive liquidity tokens or proof of their stake in return. These tokens can sometimes be staked in additional platforms to earn more rewards.

#### Risks and Considerations

- **Smart Contract Risk**: As yield farming relies on smart contracts, any bugs or vulnerabilities in the contract code can lead to loss of funds.

- **Impermanent Loss**: This occurs when the price of your deposited assets changes compared to when you deposited them. The greater the change, the more you're exposed to impermanent loss.

- **Market Risk**: The volatile nature of cryptocurrency markets can lead to significant fluctuations in yield farming returns.

### Yield Farming implementation

- The project uses Plutarch, a language for writing Plutus scripts that run on the Cardano blockchain.
- It provides functions like `pterminateYieldFarming`, `pharvestYieldFarm`, and `paddYieldFarmRewards` to manage yield farming operations.
- It includes data types `YieldFarmingDatum` and `YieldFarmRedeemer` to represent the data associated with a yield farming transaction.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment.
To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.18.1
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/yieldfarming.git
```

Navigate to the repository directory:

```sh
cd yieldfarming
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop .
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal build all
```

Execute the test suite:

```sh
cabal test
```

![yield-farming.gif](/assets/images/yield-farming.gif)

## Demeter Workspace 

To provide a seamless experience for running and trying out our application, click the workspace button below. This will start a workspace in Demeter with our repository code automatically cloned.


[![Code in Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/Anastasia-Labs/yieldfarming&template=plutus&size=large) 

## Testing

For comprehensive information on testing the Yield Farming implementation, including unit tests and property-based tests, please refer to our [test documentation](/test/README.md).

# Case study

For an in-depth real-world case study on the application of Yeild Farmings within the Cardano blockchain environment, refer to the following resource:

[Minswap - Yeild Farming](https://docs.minswap.org/min-token/yield-farming)

This case study provides valuable insights into how Yeild Farmings are integrated into blockchain transactions, offering practical examples and detailed workflows.
