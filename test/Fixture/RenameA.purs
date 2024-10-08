module Test.Fixture.RenameA
  ( func1
  , DataType(ACons, BCons)
  , Newt(Newt)
  , TypeSyn
  , class ClsA
  , toInt
  , Tup(..)
  , (/\)
  , type (/\)
  , func'
  ) where

type TypeSyn :: Type
type TypeSyn = Int

data DataType :: Type
data DataType = ACons Int | BCons

newtype Newt :: Type
newtype Newt = Newt Int

func1 :: Int -> TypeSyn
func1 int =
  int

func' :: Int
func' = 1

f = func'

newT :: Newt
newT = Newt 10

foreign import fNewT :: Newt

local1 :: Int
local1 = func1 10

class ClsA a where
  toInt :: a -> Int

data Tup a b = Tup a b

infixl 5 type Tup as /\
infixl 5 Tup as /\
