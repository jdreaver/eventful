module Bank.Aggregates.AccountSpec (spec) where

import Test.Hspec
import Eventful

import Bank.Aggregates.Account
import Bank.Commands
import Bank.Events

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
          [ Account 0 Nothing []
          , Account 10 (Just nil) []
          , Account 5 (Just nil) []
          , Account 15 (Just nil) []
          ]
      allProjections accountProjection events `shouldBe` states

    it "should handle successful account transfers" $ do
      let
        transferUuid = read "754d7bd9-fd0b-4006-b33a-f41fd5c3ca5e" :: UUID
        sourceAccount = read "afee3d24-df9b-4069-be0e-80bf0ba4f662" :: UUID
        targetAccount = read "44e9fd39-0179-4050-8706-d5a1d2c6d093" :: UUID
        events =
          [ AccountOpened' $ AccountOpened nil 10
          , AccountTransferStarted' $ AccountTransferStarted transferUuid sourceAccount 6 targetAccount
          ]
        states =
          [ Account 0 Nothing []
          , Account 10 (Just nil) []
          , Account 10 (Just nil) [PendingAccountTransfer transferUuid sourceAccount 6 targetAccount]
          ]
      allProjections accountProjection events `shouldBe` states

      let
        stateAfterStarted = latestProjection accountProjection events

      accountAvailableBalance stateAfterStarted `shouldBe` 4
      aggregateCommand accountAggregate stateAfterStarted (DebitAccount' (DebitAccount 9 "blah"))
        `shouldBe` Left (AccountCommandError' $ NotEnoughFundsError (NotEnoughFundsData 4))

      let
        events' = events ++ [AccountTransferCompleted' $ AccountTransferCompleted transferUuid]
        completedState = latestProjection accountProjection events'

      completedState `shouldBe` Account 4 (Just nil) []

  describe "Account aggregate" $ do
    it "should handle a series of commands" $ do
      let
        commands =
          [ OpenAccount' $ OpenAccount nil 100
          , DebitAccount' $ DebitAccount 150 "ATM"
          , CreditAccount' $ CreditAccount 200 "Check"
          , OpenAccount' $ OpenAccount nil 200
          ]
        results =
          [ Right $ Account 0 Nothing []
          , Right $ Account 100 (Just nil) []
          , Left $ AccountCommandError' $ NotEnoughFundsError $ NotEnoughFundsData 100
          , Right $ Account 300 (Just nil) []
          , Left $ AccountCommandError' AccountAlreadyOpenError
          ]
      allAggregateStates accountAggregate commands `shouldBe` results
