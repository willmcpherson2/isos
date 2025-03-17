-- Low-level Intermediate Representation
module Lir (Prog (..), Data (..), Fun (..), Block, Op (..), Arm (..)) where

data Prog = Prog {datas :: [Data], funs :: [Fun], entry :: Block}

data Data = Data {symbol :: Int, arity :: Int}

data Fun = Fun {symbol :: Int, arity :: Int, code :: Block}

type Block = [Op]

data Op
  = LoadData {name :: Int, symbol :: Int}
  | LoadArg {name :: Int, var :: Int, arg :: Int}
  | Copy {name :: Int, var :: Int}
  | Call {name :: Int, var :: Int}
  | ReturnTerm {var :: Int}
  | ReturnSymbol {var :: Int}
  | FreeArgs {var :: Int}
  | FreeTerm {var :: Int}
  | AppNew {name :: Int, var :: Int, args :: [Int]}
  | AppFrom {name :: Int, old :: Int, var :: Int, args :: [Int]}
  | PartialNew {name :: Int, var :: Int, args :: [Int]}
  | PartialFrom {name :: Int, old :: Int, var :: Int, args :: [Int]}
  | Match {var :: Int, arms :: [Arm]}

data Arm = Arm {symbol :: Int, code :: Block}
