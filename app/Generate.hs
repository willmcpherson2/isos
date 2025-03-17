module Generate (generate) where

import Control.Monad (when)
import Foreign (Ptr)
import Language.C.Inline.Cpp qualified as C
import Lir
import System.Exit (ExitCode (..), exitWith)

data State

C.context $ C.cppCtx <> C.cppTypePairs [("State", [t|State|])]

C.include "state.h"

check :: Ptr State -> IO ()
check state = do
  code <- fromEnum <$> [C.exp| int { $(State* state)->error } |]
  when (code > 0) $ do
    [C.exp| void { $(State* state)->printError() } |]
    exitWith $ ExitFailure code

generate :: Prog -> IO ()
generate prog = do
  state <- [C.exp| State* { new State() } |]
  check state
