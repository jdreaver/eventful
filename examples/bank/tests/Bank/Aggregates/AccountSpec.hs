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
          [ AccountOpened' $ AccountOpened nil 10
          , AccountDebited' $ AccountDebited 5 "ATM"
          , AccountCredited' $ AccountCredited 10 "Paycheck"
          ]
        states =
          [ Account 0 Nothing
          , Account 10 (Just nil)
          , Account 5 (Just nil)
          , Account 15 (Just nil)
          ]
      allProjections accountProjection events `shouldBe` states

  describe "Account aggregate" $ do
    it "should handle a series of commands" $ do
      let
        commands =
          [ OpenAccount $ OpenAccountData nil 100
          , DebitAccount $ DebitAccountData 150 "ATM"
          , CreditAccount $ CreditAccountData 200 "Check"
          , OpenAccount $ OpenAccountData nil 200
          ]
        results =
          [ Right $ Account 0 Nothing
          , Right $ Account 100 (Just nil)
          , Left $ NotEnoughFundsError $ NotEnoughFundsData 100
          , Right $ Account 300 (Just nil)
          , Left AccountAlreadyOpenError
          ]
      allAggregateStates accountAggregate commands `shouldBe` results
