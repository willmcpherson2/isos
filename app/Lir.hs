-- Low-level Intermediate Representation
module Lir (Prog (..), Data (..), Fun (..), Block, Op (..), Arm (..)) where

import Data.Vector.Storable (Vector)
import Data.Word (Word16, Word32, Word64)

type Key = Word64

type Symbol = Word32

type Arity = Word16

type Index = Word64

data Prog = Prog {datas :: [Data], funs :: [Fun], entry :: Block}

data Data = Data {name :: Key, symbol :: Symbol, arity :: Arity}

data Fun = Fun {name :: Key, argName :: Key, symbol :: Symbol, arity :: Arity, block :: Block}

type Block = [Op]

data Op
  = LoadData {name :: Key, symbol :: Symbol}
  | LoadArg {name :: Key, var :: Key, index :: Index}
  | NewApp {name :: Key, var :: Key, args :: Vector Key}
  | NewPartial {name :: Key, var :: Key, args :: Vector Key}
  | AppPartial {name :: Key, var :: Key, args :: Vector Key}
  | Copy {name :: Key, var :: Key}
  | FreeArgs {var :: Key}
  | FreeTerm {var :: Key}
  | Call {name :: Key, var :: Key}
  | ReturnTerm {var :: Key}
  | ReturnSymbol {var :: Key}
  | Match {var :: Key, arms :: [Arm]}

data Arm = Arm {symbol :: Symbol, block :: Block}
