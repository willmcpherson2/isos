module Main (main) where

import Generate
import Lir

main :: IO ()
main = do
  let idTrueProg =
        Prog
          -- main = id True
          [ App 0 Nothing [Def 2, Def 1],
            Call 1 (Var 0),
            Return (Var 1)
          ]
          [ -- False
            Data 0 0,
            -- True
            Data 0 1,
            -- id x = x
            Fun
              1
              [ Load 0 Self 0,
                Free Self,
                Call 1 (Var 0),
                Return (Var 1)
              ]
          ]
  generate idTrueProg
  pure ()
