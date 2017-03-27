{-# LANGUAGE OverloadedStrings #-}

module BlockchainSpec (spec) where

import           Blockchain
import           Json

import           Data.Aeson (decode, encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "generateNextBlock" $
    let i  = 1
        ph = bhash genesisBlock
        ts = "myTimestamp"
        bd = "myBlockdata"
        h  = calculateHash i ph ts bd
        nb = generateNextBlock genesisBlock ts bd
        nc = addBlock nb (addBlock genesisBlock emptyBlockchain)
    in do it "generate"   $ nb                           `shouldBe` Block i ph ts bd h
          it "valid"      $ isValidBlock genesisBlock nb `shouldBe` Nothing
          it "validChain" $ isValidChain nc              `shouldBe` Nothing
  describe "aeson" $
    it "decode . encode" $
      decode (encode genesisBlock) `shouldBe` Just genesisBlock
