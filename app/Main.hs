{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.C.Inline.Cpp qualified as C

C.context C.cppCtx

C.include "fac.h"

main :: IO ()
main = do
  let n = 5
  let m = [C.pure| int { fac($(int n)) } |]
  print m
