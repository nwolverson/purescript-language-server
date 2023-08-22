module Test.Fixture.RenameB where

import Test.Fixture.RenameA (class ClsA, DataType(ACons), Newt(Newt), TypeSyn, func1, toInt, Tup(..), (/\), type (/\))
import Test.Fixture.RenameA as A

dt :: A.DataType
dt = A.ACons 0

dt2 :: DataType
dt2 = ACons 0

val1 = A.func1 0

data TypeB = TypeB TypeSyn

instance ClsA TypeB where
  toInt (TypeB v) = func1 v

fnCls :: forall a. ClsA a => a -> TypeSyn
fnCls = toInt

newT :: Newt
newT = Newt 1

tup :: Int /\ String
tup = 1 /\ "1"