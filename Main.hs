{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.Classy
import Control.Exception (Exception, SomeException)
import Test.DejaFu

main :: IO Bool
main =
  autocheck do
    var :: MVar (Program Basic IO) (Either SomeException ()) <- newEmptyMVar

    threadId <-
      uninterruptibleMask \restore ->
        fork do
          result <- try (restore (throw A))
          putMVar var result

    killThread threadId

    _ <- takeMVar var

    pure ()

try :: (Exception e, MonadConc m) => m a -> m (Either e a)
try action =
  catch (Right <$> action) (pure . Left)

data A = A
  deriving stock (Show)
  deriving anyclass (Exception)
