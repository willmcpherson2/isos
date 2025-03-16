module Main (main) where

import Generate
import Lir

main :: IO ()
main = do
  let idTrueProg =
        Prog
          [ -- False
            Data 0 0,
            -- True
            Data 1 0
          ]
          [ -- id x = x
            Fun
              2
              1
              [ Index 1 0 0, -- 1 = 0[0]
                Free 0, -- free(0)
                Call 2 1, -- 2 = 1()
                Return 2 -- return 2
              ]
          ]
          -- main = id True
          [ Load 1 2, -- 1 = id
            Load 2 1, -- 2 = True
            AppNew 3 1 [2], -- 3 = appNew(1, 2)
            Call 4 3, -- 4 = 3()
            Return 4 -- return 4
          ]
  generate idTrueProg
  pure ()
