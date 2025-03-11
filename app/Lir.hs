{-# LANGUAGE DuplicateRecordFields #-}

-- Low-level Intermediate Representation
module Lir () where

type Prog = [Def]

data Def
  = Data {arity :: Int, symbol :: Int}
  | Fun {arity :: Int, code :: Block}

type Block = [Op]

data Op
  = Load {var :: Int, term :: Ref, arg :: Int}
  | Call {var :: Int, term :: Ref}
  | Switch {term :: Ref, cases :: [Case]}
  | App {arg :: Int, old :: Maybe Ref, args :: [Ref]}
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

identity :: Def
identity =
  Fun
    1
    [ Load 0 Self 0,
      Free Self,
      Call 1 (Var 0),
      Return (Var 1)
    ]

constant :: Def
constant =
  Fun
    1
    [ Load 0 Self 0,
      Load 1 Self 1,
      FreeDeep (Var 1),
      Free Self,
      Call 2 (Var 0),
      Return (Var 2)
    ]

false :: Def
false = Data 0 0

true :: Def
true = Data 0 1

not :: Def
not =
  Fun
    1
    [ Load 0 Self 0,
      Switch
        (Var 0)
        [ Case
            0
            [ Free Self,
              Return (Def 1)
            ],
          Case
            1
            [ Free Self,
              Return (Def 0)
            ]
        ]
    ]
