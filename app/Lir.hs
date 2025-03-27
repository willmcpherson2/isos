-- Low-level Intermediate Representation
module Lir (Prog (..), Data (..), Fun (..), Block, Op (..), Arm (..)) where

import Data.Vector.Storable (Vector)
import Language.C.Inline.Cpp (CInt)

data Prog = Prog {datas :: [Data], funs :: [Fun], entry :: Block}

data Data = Data {name :: CInt, symbol :: CInt, arity :: CInt}

data Fun = Fun {name :: CInt, argName :: CInt, symbol :: CInt, arity :: CInt, block :: Block}

type Block = [Op]

data Op
  = LoadData {name :: CInt, symbol :: CInt}
  | LoadArg {name :: CInt, var :: CInt, arg :: CInt}
  | NewApp {name :: CInt, var :: CInt, args :: Vector CInt}
  | NewPartial {name :: CInt, var :: CInt, args :: Vector CInt}
  | AppPartial {name :: CInt, var :: CInt, args :: Vector CInt}
  | Copy {name :: CInt, var :: CInt}
  | FreeArgs {var :: CInt}
  | FreeTerm {var :: CInt}
  | Call {name :: CInt, var :: CInt}
  | ReturnTerm {var :: CInt}
  | ReturnSymbol {var :: CInt}
  | Match {var :: CInt, arms :: [Arm]}

data Arm = Arm {symbol :: CInt, block :: Block}
