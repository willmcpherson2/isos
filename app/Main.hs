module Main (main) where

import Generate
import Lir

main :: IO ()
main = do
  let idTrueProg =
        Prog
          [ -- True
            Data 1 0,
            -- False
            Data 2 0
          ]
          [ -- id x = x
            Fun
              3
              1
              [ LoadArg 1 0 0,
                FreeArgs 0,
                Call 2 1,
                ReturnTerm 2
              ]
          ]
          -- main = id True
          [ LoadData 1 3,
            LoadData 2 1,
            NewApp 3 1 [2],
            Call 4 3,
            ReturnSymbol 4
          ]
  generate idTrueProg
  pure ()
