module Generate (generate) where

import Control.Monad (when)
import Data.Vector.Storable (thaw)
import Foreign (Ptr)
import qualified Language.C.Inline.Cpp as C
import Lir
import System.Exit (ExitCode (..), exitWith)

data State

C.context $ C.baseCtx <> C.vecCtx <> C.cppCtx <> C.cppTypePairs [("State", [t|State|])]

C.include "state.h"

check :: Ptr State -> IO ()
check state = do
  code <- fromEnum <$> [C.exp| int { $(State *state)->error } |]
  when (code > 0) $ do
    [C.exp| void { $(State *state)->printError() } |]
    exitWith $ ExitFailure code

generate :: Prog -> IO ()
generate prog = do
  state <- [C.exp| State * { new State() } |]
  check state

  mapM_ (genData state) prog.datas
  mapM_ (genFuns state) prog.funs
  genMain state prog.entry

  [C.exp| void { $(State *state)->linkRuntime() } |]
  check state

  [C.exp| void { $(State *state)->validate() } |]
  check state

  [C.exp| void { $(State *state)->optimize() } |]

  [C.exp| void { $(State *state)->writeObjectFile() } |]
  check state

  [C.exp| void { $(State *state)->linkObjectFile() } |]
  check state

genData :: Ptr State -> Data -> IO ()
genData state Data {symbol, arity} =
  [C.exp| void { $(State *state)->data($(int symbol), $(int arity)) } |]

genFuns :: Ptr State -> Fun -> IO ()
genFuns state Fun {symbol, arity, block} = do
  [C.exp| void { $(State *state)->function($(int symbol), $(int arity)) } |]
  genBlock state block

genMain :: Ptr State -> Block -> IO ()
genMain state block = do
  [C.exp| void { $(State *state)->main() } |]
  genBlock state block

genBlock :: Ptr State -> Block -> IO ()
genBlock = mapM_ . genOp

genOp :: Ptr State -> Op -> IO ()
genOp state = \case
  LoadData {name, symbol} -> [C.exp| void { $(State *state)->loadData($(int name), $(int symbol)) } |]
  LoadArg {name, var, arg} -> [C.exp| void { $(State *state)->loadArg($(int name), $(int var), $(int arg)) } |]
  Copy {name, var} -> [C.exp| void { $(State *state)->copy($(int name), $(int var)) } |]
  Call {name, var} -> [C.exp| void { $(State *state)->call($(int name), $(int var)) } |]
  ReturnTerm {var} -> [C.exp| void { $(State *state)->returnTerm($(int var)) } |]
  ReturnSymbol {var} -> [C.exp| void { $(State *state)->returnSymbol($(int var)) } |]
  FreeArgs {var} -> [C.exp| void { $(State *state)->freeArgs($(int var)) } |]
  FreeTerm {var} -> [C.exp| void { $(State *state)->freeTerm($(int var)) } |]
  AppNew {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(State *state)->appNew($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  AppFrom {name, old, var, args} -> error "todo"
  PartialNew {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(State *state)->partialNew($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  PartialFrom {name, old, var, args} -> error "todo"
  AppPartial {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(State *state)->appPartial($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  Match {var, arms} -> do
    [C.exp| void { $(State *state)->match($(int var)) } |]
    mapM_ (genArm state) arms

genArm :: Ptr State -> Arm -> IO ()
genArm state Arm {symbol, block} = do
  [C.exp| void { $(State *state)->arm($(int symbol)) } |]
  mapM_ (genOp state) block
