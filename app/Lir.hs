{-# LANGUAGE DuplicateRecordFields #-}

-- Low-level Intermediate Representation
module Lir (Prog (..), Def (..), Block, Op (..), Case (..), Ref (..)) where

data Prog = Prog {entry :: Block, defs :: [Def]}

data Def
  = Data {arity :: Int, symbol :: Int}
  | Fun {arity :: Int, code :: Block}

type Block = [Op]

data Op
  = Load {var :: Int, term :: Ref, arg :: Int}
  | Call {var :: Int, term :: Ref}
  | Switch {term :: Ref, cases :: [Case]}
  | App {var :: Int, old :: Maybe Ref, args :: [Ref]}
  | Partial {var :: Int, old :: Maybe Ref, args :: [Ref]}
  | Copy {var :: Int, term :: Ref}
  | Free {term :: Ref}
  | FreeDeep {term :: Ref}
  | Return {term :: Ref}

data Case = Case {symbol :: Int, code :: Block}

data Ref
  = Def {def :: Int}
  | Var {var :: Int}
  | Self
