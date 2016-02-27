{-# LANGUAGE FlexibleInstances     #-}   
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module EitherT 
    (
      EitherT(..)
    ) where  
    
import Control.Monad.Trans
import Control.Monad.State
import Data.Either
import Control.Applicative
import Control.Monad

newtype EitherT e m b = EitherT {
    runEitherT :: m (Either e b)
}

returnEitherT :: (Monad m) => b -> EitherT e m b
returnEitherT val = EitherT $ return (Right val)
-- returnEitherT = EitherT . return . Right

bindEitherT :: (Monad m) => EitherT e m b -> (b -> EitherT e m c) -> EitherT e m c
mv `bindEitherT` f = EitherT $
    runEitherT mv >>= \unwrapped ->
        case unwrapped of
            Left err -> return (Left err)
            Right val -> runEitherT (f val)

{- mv `bindEitherT` f = EitherT $ runEitherT mv >>= \u -> next u f
       where
           next (Right val) f = runEitherT (f val)
           next (Left err) _  = return (Left err)
-}

left :: (Monad m) => e -> EitherT e m b
left err = EitherT $ return (Left err)
-- left = EitherT . return . Left

failEitherT :: (Monad m) => String -> EitherT e m b
failEitherT msg = (EitherT . return . Left . error) msg

instance Functor f => Functor (EitherT e f) where
    fmap g mv = EitherT $ fmap (fmap g) (runEitherT mv) 

instance Applicative m => Applicative (EitherT e m) where
    --pure = EitherT . pure . Right
    pure = EitherT . pure . pure
    mf <*> mv = EitherT $ ((pure ( \v -> (<*>) v )) <*> runEitherT mf) <*> runEitherT mv
    
instance (Monad m) => Monad (EitherT e m) where
    return = returnEitherT
    (>>=)  = bindEitherT
    fail   = failEitherT
    
instance MonadTrans (EitherT e) where
    lift = EitherT . liftM Right
    
instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO
    
instance (MonadState s m) => MonadState s (EitherT e m) where
    get  = lift get
    put  = lift . put
    state = \step -> get >>= \st -> return ( fst $ step st )
    -- state = \step -> \st -> return (fst $ step st) =<< get
    
instance (Applicative m, Monoid a) => Monoid (EitherT e m a) where
    mempty = pure mempty
    mappend = liftA2 mappend
   

    
