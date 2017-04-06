module Bank.Aggregates.AccountSpec (spec) where

import Test.Hspec
import Eventful

import Bank.Aggregates.Account

spec :: Spec
spec = do
  describe "Account projection" $ do
    it "should handle a series of account events" $ do
      let
        events =
          [ AccountAccountOpened $ AccountOpened "Bob" 10
          , AccountAccountDebited $ AccountDebited 5 "ATM"
          , AccountAccountCredited $ AccountCredited 10 "Paycheck"
          ]
        states =
          [ Account 0 Nothing
          , Account 10 (Just "Bob")
          , Account 5 (Just "Bob")
          , Account 15 (Just "Bob")
          ]
      allProjections accountProjection events `shouldBe` states

  describe "Account aggregate" $ do
    it "should handle a series of commands" $ do
      let
        commands =
          [ OpenAccount $ OpenAccountData "Jen" 100
          , DebitAccount $ DebitAccountData 150 "ATM"
          , CreditAccount $ CreditAccountData 200 "Check"
          , OpenAccount $ OpenAccountData "Bob" 200
          ]
        results =
          [ Right $ Account 0 Nothing
          , Right $ Account 100 (Just "Jen")
          , Left $ NotEnoughFundsError $ NotEnoughFundsData 100
          , Right $ Account 300 (Just "Jen")
          , Left AccountAlreadyOpenError
          ]
      allAggregateStates accountAggregate commands `shouldBe` results
