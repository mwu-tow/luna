{-# LANGUAGE Strict #-}

module Luna.Runtime.Eff where

import Prologue hiding (Exception)

import qualified Control.Exception as Exception

newtype Exception = Exception Text deriving (Show, Eq)
instance Exception.Exception Exception
makeLenses ''Exception

data Eff a = Pure a
           | Monadic (IO a)
           deriving (Functor)

instance Applicative Eff where
    pure = Pure
    {-# INLINE pure #-}

    (<*>) = \case
        Pure f -> \case
            Pure a    -> Pure $ f a
            Monadic a -> Monadic $ f <$> a
        Monadic f -> \case
            Pure a    -> Monadic $ (\f -> f a) <$> f
            Monadic a -> Monadic $ f <*> a
    {-# INLINE (<*>) #-}

instance Monad Eff where
    (>>=) = \case
        Pure a -> \f -> f a
        Monadic a -> \f -> Monadic $ a >>= \a' -> case f a' of
            Pure r -> pure r
            Monadic r -> r
    {-# INLINE (>>=) #-}

instance MonadFix Eff where
    mfix = unsafeLiftIO . mfix . fmap runIO
    {-# INLINE mfix #-}

unsafeLiftIO :: IO a -> Eff a
unsafeLiftIO = Monadic
{-# INLINE unsafeLiftIO #-}

instance MonadIO Eff where
    liftIO = \a -> do
        let handle :: SomeException -> IO (Either Text a)
            handle = pure . Left . convert . displayException
        res <- unsafeLiftIO $ Exception.catch (Right <$> a) handle
        case res of
            Left a  -> throw a
            Right r -> return r
    {-# INLINE liftIO #-}

runIO :: Eff a -> IO a
runIO = \case
    Pure    a -> pure a
    Monadic a -> a
{-# INLINE runIO #-}

runError :: Eff a -> Eff (Either Exception a)
runError = \case
    Pure    a -> pure $ Right a
    Monadic a -> liftIO $ Exception.try a
{-# INLINE runError #-}

throw :: Text -> Eff a
throw = unsafeLiftIO . Exception.throwIO . Exception
{-# INLINE throw #-}
