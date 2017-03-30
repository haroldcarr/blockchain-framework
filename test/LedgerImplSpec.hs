{-# LANGUAGE OverloadedStrings #-}

module LedgerImplSpec (spec) where

import           LedgerImpl

import           Data.Aeson    (decode, encode)
import           Data.Sequence as S (empty)
import           Test.Hspec


spec :: Spec
spec = do
  describe "generateNextEntry" $
    let i  = 1
        ph = ehash genesisLedgerEntry
        ts = "myTimestamp"
        ed = "myBlockdata"
        h  = calculateHash i ph ts ed
        ne = generateNextLedgerEntry genesisLedger ts ed
        nl = addLedgerEntry ne (addLedgerEntry genesisLedgerEntry S.empty)
    in do it "generate"    $ ne                                       `shouldBe` LedgerEntryImpl i ph ts ed h
          it "validEntry"  $ isValidLedgerEntry genesisLedgerEntry ne `shouldBe` Nothing
          it "validLedger" $ isValidLedger nl                         `shouldBe` Nothing
          it "getEntry' 0" $ getEntry nl 0                            `shouldBe` Just ne
          it "getEntry' 1" $ getEntry nl 1                            `shouldBe` Just genesisLedgerEntry
          it "validEntryTC"$ isValidEntry genesisLedger ne            `shouldBe` Nothing
          it "validXXtryTC"$ isValidEntry nl            ne            `shouldBe` (Just "invalid bindex")
  -------------------------
  describe "aeson" $
    it "decode . encode" $
      decode (encode genesisLedgerEntry) `shouldBe` Just genesisLedgerEntry
