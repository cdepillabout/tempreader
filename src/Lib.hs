{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Reader r a = Reader { unReader :: (r -> a) }


lala :: Reader Int ()
lala = Reader $ \int -> ()

runReader :: Reader r a -> (r -> a)
runReader (Reader rToa) = rToa

runLala :: ()
runLala = runReader lala 1

instance Functor (Reader r) where
  -- fmap :: (a -> b) -> f a -> f b
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap aTob (Reader rToa) = Reader $ \r -> aTob (rToa r)

fmapTest :: Reader Int String -> Reader Int Int
fmapTest readerIntString = fmap (\string -> length string) readerIntString

fmapTest' :: (Int -> String) -> (Int -> Int)
fmapTest' intToString = \int -> length $ intToString int

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \_ -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader rToaTob) (Reader rToa) = Reader $ \r ->
    let a = rToa r
    in rToaTob r a

instance Monad (Reader r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader rToa) aToReaderrb = Reader $ \r ->
    let a = rToa r
        (Reader rTob) = aToReaderrb a
    in rTob r


data Config = Config Int deriving (Eq, Read, Show)

data HowBig = Big | Small deriving (Eq, Read, Show)

myFuncHowBig :: Config -> HowBig
myFuncHowBig (Config int) = if int > 1000 then Big else Small

myFuncAlwaysBig :: Config -> HowBig
myFuncAlwaysBig _ = Big

myTwoFuncs :: Config -> (HowBig, HowBig)
myTwoFuncs config = (myFuncHowBig config, myFuncAlwaysBig config)

myFuncHowBigReader :: Reader Config HowBig
myFuncHowBigReader = Reader $ \(Config int) ->
  if int > 1000 then Big else Small

myFuncAlwaysBigReader :: Reader Config HowBig
myFuncAlwaysBigReader = Reader $ const Big

myTwoFuncsReaderExample :: Reader Config (HowBig, HowBig)
myTwoFuncsReaderExample = do
  (undefined :: String) <- (undefined :: Reader Config String)
  -- (undefined :: Int) <- (undefined :: Reader Config Int)
  pure (Big, Big) :: Reader Config (HowBig, HowBig)

myTwoFuncsReader :: Reader Config (HowBig, HowBig)
myTwoFuncsReader = do
  howBigOne <- myFuncHowBigReader
  howBigTwo <- myFuncAlwaysBigReader
  pure (howBigOne, howBigTwo)

-- instance Monad ((->) r) where
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
  -- (>>=) :: r -> (r -> a) -> (a -> r -> b) -> b

myTwoFuncsConfusing :: Config -> (HowBig, HowBig)
myTwoFuncsConfusing = do
  howBigOne <- myFuncHowBig
  howBigTwo <- myFuncAlwaysBig
  pure (howBigOne, howBigTwo)

myMain :: IO  ()
myMain =
  let howBigs = unReader myTwoFuncsReader (Config 1)
  in print howBigs


newtype Op a b = Op { getOp :: b -> a }
-- newtype Reader r a = Reader { unReader :: r -> a }

-- instace Functor (Op a) where
--   fmap :: (x -> y) -> f x -> f y
--   fmap :: (x -> y) -> Op a x -> Op a y
--   fmap xToy (Op xToa) = Op $ \y -> undefined

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant (Op x) where
  contramap :: (a -> b) -> Op x b -> Op x a
  contramap aTob (Op bTox) = Op $ \a -> bTox (aTob a)

