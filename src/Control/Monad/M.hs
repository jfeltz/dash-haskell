module Control.Monad.M where
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative ((<$>))
import System.Exit
import Pipes

import Data.String.Indent

type M r = ReaderT Env (EitherT String IO) r

data Env = Env { indention :: Int, verbosity :: Bool }

newEnv :: Bool -> Env  
newEnv = Env 0
     
env :: M Env 
env = ask 
    
warning :: String -> M () 
warning str = do 
  i <- indention <$> env 
  liftIO . putStr $ fromIndent ("warning: " ++ str) i

indent :: Int -> Env -> Env
indent amount (Env i v) = Env (i + amount) v

indentM :: Int -> M r -> M r
indentM amount = local (indent amount) 

msg :: (MonadIO m) => String -> ReaderT Env m () 
msg str = do
  e <- ask 
  when (verbosity e) . liftIO . putStr . fromIndent str $ indention e

err :: String -> M r
err = lift . left

runM :: Env -> M () -> IO () 
runM env' comp = do 
  result <- runEitherT (runReaderT comp env')
  case result of
    Left e -> do 
      liftIO . putStrLn $ "fatal error:\n\n" ++ e
      exitFailure
    Right ()  -> 
      exitSuccess

type ConsumerM i r = Consumer i (ReaderT Env (EitherT String IO)) r
type ProducerM i r = Producer i (ReaderT Env (EitherT String IO)) r
type PipeM a b r = Pipe a b (ReaderT Env (EitherT String IO)) r
