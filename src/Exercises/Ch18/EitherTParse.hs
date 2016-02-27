{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
            
module EitherTParse
    (
      ParseE
    , evalParseE
    ) where  
            
import EitherT
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

data ParseState = 
    ParseState 
    {
      string :: L.ByteString
    , offset :: Int64
    } 
    deriving (Show)

-- This class is broken. The way EitherT is implemented is not compatible
-- with making ParseE a functor.
newtype ParseE a e = ParseE {
      runParseE :: EitherT e (State ParseState) a
    } --deriving (Monad, MonadState ParseState)
    
{-
instance Functor (ParseE a) where
    --fmap :: c -> d -> ParseE a c -> ParseE a d
    fmap f pv = ParseE $ EitherT $ state $ \st -> (step st, st)
        where
            parse = runState $ runEitherT $ runParseE pv
            step  = fmap f . fst . parse
-}
--instance Applicative (ParseE a) where
--    pure
--    <*>

evalParseE :: ParseE a e -> L.ByteString -> Either e a
evalParseE m s = evalState (runEitherT (runParseE m)) (ParseState s 0)
