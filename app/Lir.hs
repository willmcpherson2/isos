-- Low-level Intermediate Representation
module Lir (Prog (..), Data (..), Fun (..), Block, Op (..), Arm (..)) where

import Data.Vector.Storable (Vector)
import Language.C.Inline.Cpp (CInt)

data Prog = Prog {datas :: [Data], funs :: [Fun], entry :: Block}

data Data = Data {symbol :: CInt, arity :: CInt}

data Fun = Fun {symbol :: CInt, arity :: CInt, block :: Block}

type Block = [Op]

data Op
  = LoadData {name :: CInt, symbol :: CInt}
  | LoadArg {name :: CInt, var :: CInt, arg :: CInt}
  | Copy {name :: CInt, var :: CInt}
  | Call {name :: CInt, var :: CInt}
  | ReturnTerm {var :: CInt}
  | ReturnSymbol {var :: CInt}
  | FreeArgs {var :: CInt}
  | FreeTerm {var :: CInt}
  | AppNew {name :: CInt, var :: CInt, args :: Vector CInt}
  | AppFrom {name :: CInt, old :: CInt, var :: CInt, args :: Vector CInt}
  | PartialNew {name :: CInt, var :: CInt, args :: Vector CInt}
  | PartialFrom {name :: CInt, old :: CInt, var :: CInt, args :: Vector CInt}
  | AppPartial {name :: CInt, var :: CInt, args :: Vector CInt}
  | Match {var :: CInt, arms :: [Arm]}

data Arm = Arm {symbol :: CInt, block :: Block}
