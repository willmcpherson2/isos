module Main (main) where

import Generate
import Lir

main :: IO ()
main = do
  let idTrueProg =
        Prog
          [ -- False
            Data 0 0
          , -- True
            Data 1 0
          ]
          [ -- id x = x
            Fun
              2
              1
              [ LoadArg 1 0 0 -- 1 = 0.args[0]
              , FreeArgs 0 -- free(0.args)
              , Call 2 1 -- 2 = 1.fun(1)
              , ReturnTerm 2 -- return 2
              ]
          ]
          -- main = id True
          [ LoadData 1 2 -- 1 = id
          , LoadData 2 1 -- 2 = True
          , AppNew 3 1 [2] -- 3 = appNew(1, 2)
          , Call 4 3 -- 4 = 3.fun(3)
          , ReturnSymbol 4 -- return 4.symbol
          ]
  generate idTrueProg
  pure ()
