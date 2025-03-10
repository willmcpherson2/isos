{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Foreign.Ptr (Ptr)
import Language.C.Inline.Cpp qualified as C

data State

C.context $ C.cppCtx <> C.cppTypePairs [("State", [t|State|])]

C.include "state.h"

main :: IO ()
main = do
  state <- [C.exp| State* { new State() } |] :: IO (Ptr State)
  n1 <- [C.exp| int { $(State* state)->get() } |]
  print n1
  [C.exp| void { $(State* state)->set(1) } |]
  n2 <- [C.exp| int { $(State* state)->get() } |]
  print n2
