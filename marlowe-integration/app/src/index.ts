#!/usr/bin/env node
import yargs from 'yargs/yargs'
import { hideBin } from 'yargs/helpers'
import integration from './integration.js'
import query from './query.js'

yargs(hideBin(process.argv))
  .scriptName('marlowe-integration')
  .command(integration)
  .command(query)
  .demandCommand(1, 'You need to specify a command')
  .help()
  .parse()

