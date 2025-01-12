module Language.Marlowe.Runtime.CLI.Command.Tx where

import qualified Data.Set as Set
import Language.Marlowe.Runtime.CLI.Option (parseAddress, txOutRefParser)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (WalletAddresses))
import Options.Applicative

data TxCommand cmd = TxCommand
  { walletAddresses :: WalletAddresses
  , signingMethod :: SigningMethod
  , metadataFile :: Maybe FilePath
  , tagsFile :: Maybe FilePath
  , subCommand :: cmd
  }

newtype SigningMethod
  = Manual FilePath
  deriving (Show)

txCommandParser :: Bool -> Parser cmd -> Parser (TxCommand cmd)
txCommandParser metadataSupported subCommandParser =
  TxCommand
    <$> walletAddressesParser
    <*> signingMethodParser
    <*> metadataFileParser
    <*> tagsFileParser
    <*> subCommandParser
  where
    walletAddressesParser =
      WalletAddresses
        <$> changeAddressParser
        <*> extraAddressesParser
        <*> collateralUtxosParser
    -- TODO add other signing methods with <|> here (e.g. CIP-30, cardano-wallet).
    signingMethodParser = manualSignParser
    manualSignParser =
      fmap Manual $
        strOption $
          mconcat
            [ long "manual-sign"
            , metavar "FILE_PATH"
            , help
                "Sign the transaction manually. Writes the CBOR bytes of the unsigned transaction to the specified file for manual signing. Use the submit command to submit the signed transaction."
            ]
    metadataFileParser
      | metadataSupported =
          optional $
            strOption $
              mconcat
                [ long "metadata-file"
                , short 'm'
                , help
                    "A JSON file containing a map of integer indexes to arbitrary JSON values that will be added to the transaction's metadata."
                , metavar "FILE_PATH"
                ]
      | otherwise = pure Nothing
    tagsFileParser
      | metadataSupported =
          optional $
            strOption $
              mconcat
                [ long "tags-file"
                , help
                    "A JSON file containing a map of tags indexes to optional JSON-encoded metadata values that will be added to the transaction's 1564 metadata key. Note that the entire 1564 key will be overridden if also specified in --metadata-file."
                , metavar "FILE_PATH"
                ]
      | otherwise = pure Nothing
    changeAddressParser =
      option (eitherReader parseAddress) $
        mconcat
          [ long "change-address"
          , help "The address to which the change of the transaction should be sent."
          , metavar "ADDRESS"
          ]
    extraAddressesParser =
      fmap Set.fromList $
        many $
          option (eitherReader parseAddress) $
            mconcat
              [ long "address"
              , short 'a'
              , help "An address whose UTXOs can be used as inputs to the transaction"
              , metavar "ADDRESS"
              ]
    collateralUtxosParser =
      fmap Set.fromList $
        many $
          option txOutRefParser $
            mconcat
              [ long "collateral-utxo"
              , help "A UTXO which may be used as a collateral input"
              , metavar "UTXO"
              ]
