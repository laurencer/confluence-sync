{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Confluence.Sync.Internal.RateLimiter (
  Throttle
, newThrottle
, runThrottledAction
) where

import           Data.Int

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Fix
import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.MSemN    as Sem

import           System.Clock

data Throttle = Throttle {
  -- Current # of actions performed
  actionCount       :: TVar.TVar Int
  -- Total # of actions allowed
, actionLimit       :: Int
  -- Provides rate-limiting
, actionSem         :: Sem.MSemN Int
  -- Minimum time to wait after a request has finished before starting the next.
, minDelayInMillis  :: Int
  -- Maximum time to wait after a request has finished before starting the next.
, maxDelayInMillis  :: Int
  -- Multiplier for the request time to calculate a possible delay.
, requestBackoff    :: Rational
}

-- | Creates a new Throttle.
newThrottle :: Int -> Int -> Int -> Rational -> IO Throttle
newThrottle actionLimit minDelayInMillis maxDelayInMillis requestBackoff = do 
  -- Initialise an empty action counter
  actionCount  <- TVar.newTVarIO 0
  -- Create the semaphore/throttle allowing a single action.
  actionSem    <- Sem.new 2
  return $ Throttle { .. }

-- | Runs the action but ensures that it is throttled appropriately.
-- 
-- Specifically this limits the number of actions that can be invoked overall and
-- also adds in a delay between actions.
--
-- The delay is equal to the greater of the minimum delay specified and the last action time
-- up to the limit of the maximum delay.
--
-- For example if the last action took 4 seconds and the min delay was 100 ms then it would
-- wait at least another 4 seconds before continuing.
--
runThrottledAction :: Throttle -> IO a -> IO a
runThrottledAction Throttle { actionCount, actionLimit, actionSem, minDelayInMillis, maxDelayInMillis, requestBackoff } action = do
  overLimit <- liftIO $ atomically $ do
                  currentCount <- readTVar actionCount
                  if (currentCount >= actionLimit)
                    then return True
                    else (writeTVar actionCount (currentCount + 1)) *> (return False)
  if overLimit 
    then error $ "Too many actions attempted (> " ++ (show actionLimit) ++ ")"
    else do
      liftIO $ Sem.wait actionSem 1
      startTime <- liftIO $ getTime Monotonic
      !result   <- action `finally` (do
          endTime   <- liftIO $ getTime Monotonic
          let actionTimeInNs :: Int64
              actionTimeInNs = (nsec endTime) - (nsec startTime)
              actionTimeInMs :: Int64
              actionTimeInMs = ceiling ((fromIntegral actionTimeInNs) / 1000000)
              backedOffDelayInMs :: Int64
              backedOffDelayInMs = ceiling (requestBackoff * (fromIntegral actionTimeInMs))
              actionTimeOrDelay :: Int64
              actionTimeOrDelay = (max (fromIntegral minDelayInMillis) backedOffDelayInMs)
              delay :: Int64
              delay = min actionTimeOrDelay (fromIntegral maxDelayInMillis)
          liftIO $ threadDelay $ fromIntegral (delay * 1000)
          liftIO $ Sem.signal actionSem 1
        )
      return result
