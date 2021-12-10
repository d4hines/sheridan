# Vern

On-chain cooperative random number generation for Tezos.

## What is it?

Vern consists of two parts:
- A simple smart contract that allows multiple parties to
cooperate in creating random numbers on-chain. Other smart
contracts can then view number from participants (called "players")
they trust using an [on-chain view]() feature introduced in Hangzhou.
The view aggregates the numbers from the resulting number
is as random as most random trusted player. The contract
is thus like an oracle for random numbers, but allows users to
decentralize their trust across as many players as they
desire.
- An automated player in Typescript using the Taquito library

## Build

With docker installed:
```
docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.31.0 compile contract ./contract/vern.mligo --protocol hangzhou
```
