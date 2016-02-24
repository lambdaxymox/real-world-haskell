{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
            UndecidableInstances, GeneralizedNewtypeDeriving #-}
            
module Parse
    (
      ParseE
    , execParseE
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

newtype ParseE a e = ParseE {
      runParseE :: EitherT e (State ParseState) a
    } deriving (Monad, MonadState ParseState)
    
evalParseE :: ParseE a e -> L.ByteString -> Either e a
evalParseE m s = evalState (runEitherT (runParseE m)) (ParseState s 0)
