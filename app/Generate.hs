module Generate (generate) where

import Control.Monad (when)
import Data.Vector.Storable (thaw)
import Foreign (Ptr)
import qualified Language.C.Inline.Cpp as C
import Lir
import System.Exit (ExitCode (..), exitWith)

data State

C.context $ C.baseCtx <> C.vecCtx <> C.cppCtx <> C.cppTypePairs [("StateInt", [t|State|])]

C.include "state.h"

check :: Ptr State -> IO ()
check state = do
  code <- fromEnum <$> [C.exp| int { $(StateInt *state)->error } |]
  when (code > 0) $ do
    [C.exp| void { $(StateInt *state)->printError() } |]
    exitWith $ ExitFailure code

generate :: Prog -> IO ()
generate prog = do
  state <- [C.exp| StateInt * { new StateInt() } |]
  check state

  mapM_ (genData state) prog.datas
  mapM_ (genFuns state) prog.funs
  genMain state prog.entry

  [C.exp| void { $(StateInt *state)->linkRuntime() } |]
  check state

  [C.exp| void { $(StateInt *state)->validate() } |]
  check state

  [C.exp| void { $(StateInt *state)->optimize() } |]

  result <- [C.exp| int { $(StateInt *state)->jit() } |]
  putStrLn $ "main: " <> show result

genData :: Ptr State -> Data -> IO ()
genData state Data {name, symbol, arity} =
  [C.exp| void { $(StateInt *state)->data($(int name), $(int symbol), $(int arity)) } |]

genFuns :: Ptr State -> Fun -> IO ()
genFuns state Fun {name, argName, symbol, arity, block} = do
  [C.exp| void { $(StateInt *state)->function($(int name), $(int argName), $(int symbol), $(int arity)) } |]
  genBlock state block

genMain :: Ptr State -> Block -> IO ()
genMain state block = do
  [C.exp| void { $(StateInt *state)->main() } |]
  genBlock state block

genBlock :: Ptr State -> Block -> IO ()
genBlock = mapM_ . genOp

genOp :: Ptr State -> Op -> IO ()
genOp state = \case
  LoadData {name, symbol} -> [C.exp| void { $(StateInt *state)->loadData($(int name), $(int symbol)) } |]
  LoadArg {name, var, arg} -> [C.exp| void { $(StateInt *state)->loadArg($(int name), $(int var), $(int arg)) } |]
  NewApp {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->newApp($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  NewPartial {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->newPartial($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  AppPartial {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->appPartial($(int name), $(int var), $vec-len:mArgs, $vec-ptr:(int *mArgs)) } |]
  Copy {name, var} -> [C.exp| void { $(StateInt *state)->copy($(int name), $(int var)) } |]
  FreeArgs {var} -> [C.exp| void { $(StateInt *state)->freeArgs($(int var)) } |]
  FreeTerm {var} -> [C.exp| void { $(StateInt *state)->freeTerm($(int var)) } |]
  Call {name, var} -> [C.exp| void { $(StateInt *state)->call($(int name), $(int var)) } |]
  ReturnTerm {var} -> [C.exp| void { $(StateInt *state)->returnTerm($(int var)) } |]
  ReturnSymbol {var} -> [C.exp| void { $(StateInt *state)->returnSymbol($(int var)) } |]
  Match {var, arms} -> do
    [C.exp| void { $(StateInt *state)->match($(int var)) } |]
    mapM_ (genArm state) arms

genArm :: Ptr State -> Arm -> IO ()
genArm state Arm {symbol, block} = do
  [C.exp| void { $(StateInt *state)->arm($(int symbol)) } |]
  mapM_ (genOp state) block
