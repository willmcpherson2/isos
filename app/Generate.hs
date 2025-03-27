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
  code <- fromEnum <$> [C.exp| uint8_t { $(StateInt *state)->error } |]
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

  result <- [C.exp| int32_t { $(StateInt *state)->jit() } |]
  putStrLn $ "main: " <> show result

genData :: Ptr State -> Data -> IO ()
genData state Data {name, symbol, arity} =
  [C.exp| void { $(StateInt *state)->data($(uint64_t name), $(uint32_t symbol), $(uint16_t arity)) } |]

genFuns :: Ptr State -> Fun -> IO ()
genFuns state Fun {name, argName, symbol, arity, block} = do
  [C.exp| void { $(StateInt *state)->function($(uint64_t name), $(uint64_t argName), $(uint32_t symbol), $(uint16_t arity)) } |]
  genBlock state block

genMain :: Ptr State -> Block -> IO ()
genMain state block = do
  [C.exp| void { $(StateInt *state)->main() } |]
  genBlock state block

genBlock :: Ptr State -> Block -> IO ()
genBlock = mapM_ . genOp

genOp :: Ptr State -> Op -> IO ()
genOp state = \case
  LoadData {name, symbol} -> [C.exp| void { $(StateInt *state)->loadData($(uint64_t name), $(uint32_t symbol)) } |]
  LoadArg {name, var, index} -> [C.exp| void { $(StateInt *state)->loadArg($(uint64_t name), $(uint64_t var), $(uint64_t index)) } |]
  NewApp {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->newApp($(uint64_t name), $(uint64_t var), $vec-len:mArgs, $vec-ptr:(uint64_t *mArgs)) } |]
  NewPartial {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->newPartial($(uint64_t name), $(uint64_t var), $vec-len:mArgs, $vec-ptr:(uint64_t *mArgs)) } |]
  AppPartial {name, var, args} -> do
    mArgs <- thaw args
    [C.exp| void { $(StateInt *state)->appPartial($(uint64_t name), $(uint64_t var), $vec-len:mArgs, $vec-ptr:(uint64_t *mArgs)) } |]
  Copy {name, var} -> [C.exp| void { $(StateInt *state)->copy($(uint64_t name), $(uint64_t var)) } |]
  FreeArgs {var} -> [C.exp| void { $(StateInt *state)->freeArgs($(uint64_t var)) } |]
  FreeTerm {var} -> [C.exp| void { $(StateInt *state)->freeTerm($(uint64_t var)) } |]
  Call {name, var} -> [C.exp| void { $(StateInt *state)->call($(uint64_t name), $(uint64_t var)) } |]
  ReturnTerm {var} -> [C.exp| void { $(StateInt *state)->returnTerm($(uint64_t var)) } |]
  ReturnSymbol {var} -> [C.exp| void { $(StateInt *state)->returnSymbol($(uint64_t var)) } |]
  Match {var, arms} -> do
    [C.exp| void { $(StateInt *state)->match($(uint64_t var)) } |]
    mapM_ (genArm state) arms

genArm :: Ptr State -> Arm -> IO ()
genArm state Arm {symbol, block} = do
  [C.exp| void { $(StateInt *state)->arm($(uint32_t symbol)) } |]
  mapM_ (genOp state) block
