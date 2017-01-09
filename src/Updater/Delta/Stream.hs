{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Updater.Delta.Stream
    where

import           Foundation
import           Control.Applicative (Alternative(..))
import qualified Data.ByteString.Lazy as L

newtype Stream bs a = Stream { runStream :: bs -> Either String (a, bs) }

consume :: Int64 -> Stream L.ByteString L.ByteString
consume n = Stream $ \bs ->
    case L.splitAt n bs of
        (l1, l2)
            | L.length l1 == n -> Right (l1, l2)
            | otherwise        -> Left ("consuming " <> show n <> " but got: " <> show (L.length l1))

remaining :: Stream L.ByteString L.ByteString
remaining = Stream $ \bs -> Right (bs, L.empty)

instance Functor (Stream elem) where
    fmap f s = Stream $ \e1 -> case runStream s e1 of
        Left err     -> Left err
        Right (a,e2) -> Right (f a, e2)
instance Applicative (Stream elem) where
    pure  = return
    fab <*> fa = Stream $ \e1 -> case runStream fab e1 of
        Left err      -> Left err
        Right (f, e2) -> either Left (Right . first f) $ runStream fa e2
instance Alternative (Stream elem) where
    empty     = Stream $ \_  -> Left "empty"
    f1 <|> f2 = Stream $ \e1 -> either (\_ -> runStream f2 e1) Right $ runStream f1 e1
instance Monad (Stream elem) where
    return a  = Stream $ \e1 -> Right (a, e1)
    ma >>= mb = Stream $ \e1 -> either Left (\(a, e2) -> runStream (mb a) e2) $ runStream ma e1
