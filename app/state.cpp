#include "state.h"

#include "llvm/IR/GlobalVariable.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Linker/Linker.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include <iostream>
#include <sstream>

State::State()
  : mod(initMod()),
    targetMachine(initTargetMachine()),
    termType(initTermType()),
    funType(initFunType()),
    noopFun(initNoopFun()),
    freeFun(initFreeFun()),
    appNewFun(initAppNewFun()) {}

llvm::Module State::initMod() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  return llvm::Module("main", context);
}

std::unique_ptr<llvm::TargetMachine> State::initTargetMachine() {
  std::string targetTriple = llvm::sys::getDefaultTargetTriple();
  mod.setTargetTriple(targetTriple);

  auto target = llvm::TargetRegistry::lookupTarget(targetTriple, message);
  if (!target) {
    error = NoTargetTriple;
    return nullptr;
  }

  auto cpu = "generic";
  auto features = "";
  llvm::TargetOptions opt;
  auto rm = std::optional<llvm::Reloc::Model>(llvm::Reloc::PIC_);
  std::unique_ptr<llvm::TargetMachine> targetMachine(
    target->createTargetMachine(targetTriple, cpu, features, opt, rm)
  );

  mod.setDataLayout(targetMachine->createDataLayout());

  return targetMachine;
}

llvm::StructType *State::initTermType() {
  llvm::StructType *termTy = llvm::StructType::create(context, "Term");
  std::vector<llvm::Type *> termElements = {
    llvm::PointerType::get(context, 0), // fun
    llvm::PointerType::get(context, 0), // args
    llvm::Type::getInt32Ty(context),    // symbol
    llvm::Type::getInt16Ty(context),    // length
    llvm::Type::getInt16Ty(context),    // capacity
  };
  termTy->setBody(termElements);
  return termTy;
}

llvm::FunctionType *State::initFunType() {
  return llvm::FunctionType::get(termType, {termType}, false);
}

llvm::Function *State::initNoopFun() {
  auto name = "noop";

  llvm::FunctionType *funType =
    llvm::FunctionType::get(termType, {termType}, false);
  fun =
    llvm::Function::Create(funType, llvm::Function::PrivateLinkage, name, mod);

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  builder.emplace(block);

  llvm::Value *arg = fun->getArg(0);
  builder->CreateRet(arg);

  return fun;
}

llvm::Function *State::initFreeFun() {
  auto name = "free";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context), {llvm::PointerType::get(context, 0)}, false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

llvm::Function *State::initAppNewFun() {
  auto name = "app_new";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    termType,
    {termType,
     llvm::Type::getInt32Ty(context),
     llvm::PointerType::get(context, 0)},
    false
  );
  llvm::Function *fun =
    llvm::Function::Create(funType, llvm::Function::PrivateLinkage, name, mod);

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  llvm::IRBuilder<> builder(block);

  llvm::Value *funArg = fun->getArg(0);
  llvm::Value *lengthArg = fun->getArg(1);
  llvm::Value *argsArg = fun->getArg(2);

  llvm::DataLayout dataLayout(&mod);
  llvm::TypeSize termTypeSize = dataLayout.getTypeAllocSize(termType);
  llvm::Value *sizeOfTerm = builder.getInt32(termTypeSize);
  llvm::Value *size = builder.CreateMul(lengthArg, sizeOfTerm);

  llvm::FunctionType *mallocType = llvm::FunctionType::get(
    llvm::PointerType::get(context, 0), {builder.getInt32Ty()}, false
  );
  llvm::FunctionCallee mallocFun =
    mod.getOrInsertFunction("malloc", mallocType);
  llvm::Value *allocatedMemory = builder.CreateCall(mallocFun, {size});

  llvm::Value *resultAlloca = builder.CreateAlloca(termType, nullptr);
  builder.CreateStore(funArg, resultAlloca);

  llvm::Value *argsFieldPtr =
    builder.CreateStructGEP(termType, resultAlloca, 1);

  builder.CreateStore(allocatedMemory, argsFieldPtr);

  llvm::FunctionType *memcpyType = llvm::FunctionType::get(
    llvm::PointerType::get(context, 0),
    {llvm::PointerType::get(context, 0),
     llvm::PointerType::get(context, 0),
     builder.getInt32Ty()},
    false
  );
  llvm::FunctionCallee memcpyFun =
    mod.getOrInsertFunction("memcpy", memcpyType);

  builder.CreateCall(memcpyFun, {allocatedMemory, argsArg, size});

  llvm::Value *finalTerm = builder.CreateLoad(termType, resultAlloca);

  builder.CreateRet(finalTerm);

  return fun;
}

void State::main() {
  auto name = "main";

  llvm::FunctionType *funType =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {}, false);
  fun =
    llvm::Function::Create(funType, llvm::Function::ExternalLinkage, name, mod);

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  builder.emplace(block);

  values.clear();
}

void State::function(int symbol, int arity) {
  auto funName = "fun_" + std::to_string(symbol);

  llvm::FunctionType *funType =
    llvm::FunctionType::get(termType, {termType}, false);
  fun = llvm::Function::Create(
    funType, llvm::Function::PrivateLinkage, funName, mod
  );

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  builder.emplace(block);

  llvm::Value *arg = fun->getArg(0);
  values.clear();
  values.insert({0, arg});

  auto dataName = "data_" + std::to_string(symbol);

  std::vector<llvm::Constant *> fieldValues = {
    fun,                                                                // fun
    llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0)), // args
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), symbol), // symbol
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity),  // length
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity)   // capacity
  };
  llvm::Constant *termInit = llvm::ConstantStruct::get(termType, fieldValues);

  auto global = new llvm::GlobalVariable(
    mod,                               // Module
    termType,                          // Type
    true,                              // isConstant
    llvm::GlobalValue::PrivateLinkage, // Linkage
    termInit,                          // Initializer
    dataName,                          // Name
    nullptr,                           // InsertBefore
    llvm::GlobalValue::NotThreadLocal, // ThreadLocalMode
    0,                                 // AddressSpace
    false                              // isExternallyInitialized
  );

  globals.insert({symbol, global});
}

void State::data(int symbol, int arity) {
  auto name = "data_" + std::to_string(symbol);

  std::vector<llvm::Constant *> fieldValues = {
    noopFun,                                                            // fun
    llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0)), // args
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), symbol), // symbol
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity),  // length
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity)   // capacity
  };
  llvm::Constant *termInit = llvm::ConstantStruct::get(termType, fieldValues);

  auto global = new llvm::GlobalVariable(
    mod,                               // Module
    termType,                          // Type
    true,                              // isConstant
    llvm::GlobalValue::PrivateLinkage, // Linkage
    termInit,                          // Initializer
    name,                              // Name
    nullptr,                           // InsertBefore
    llvm::GlobalValue::NotThreadLocal, // ThreadLocalMode
    0,                                 // AddressSpace
    false                              // isExternallyInitialized
  );

  globals.insert({symbol, global});
}

void State::load(int name, int symbol) {
  llvm::GlobalVariable *global = globals[symbol];

  llvm::AllocaInst *allocaValue = builder->CreateAlloca(termType, nullptr);
  llvm::LoadInst *globalValue = builder->CreateLoad(termType, global);
  builder->CreateStore(globalValue, allocaValue);
  llvm::Value *value = builder->CreateLoad(termType, allocaValue);

  values.insert({name, value});
}

void State::appNew(int name, int var, int length, int *args) {
  llvm::Value *varValue = values[var];

  llvm::ConstantInt *lengthValue =
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), length);

  llvm::ArrayType *argsType = llvm::ArrayType::get(termType, length);
  llvm::AllocaInst *argsValue = builder->CreateAlloca(argsType, nullptr);
  for (int i = 0; i < length; ++i) {
    int arg = args[i];
    llvm::Value *argValue = values[arg];

    llvm::Value *element = builder->CreateGEP(
      argsType,
      argsValue,
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i)}
    );
    builder->CreateStore(argValue, element);
  }

  std::vector<llvm::Value *> argValues;
  argValues.push_back(varValue);
  argValues.push_back(lengthValue);
  argValues.push_back(argsValue);
  llvm::Value *value = builder->CreateCall(appNewFun, argValues);

  values.insert({name, value});
}

void State::call(int name, int var) {
  llvm::Value *varValue = values[var];

  llvm::Value *fun = builder->CreateExtractValue(varValue, 0);
  llvm::Value *value = builder->CreateCall(funType, fun, {varValue});

  values.insert({name, value});
}

void State::free(int var) {
  llvm::Value *varValue = values[var];

  llvm::Value *argsField = builder->CreateExtractValue(varValue, 1);
  builder->CreateCall(freeFun, {argsField});
}

void State::index(int name, int var, int i) {
  llvm::Value *varValue = values[var];

  llvm::Value *argsField = builder->CreateExtractValue(varValue, 1);
  llvm::ConstantInt *argIndex =
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i);
  llvm::Value *argPtr = builder->CreateGEP(termType, argsField, argIndex);

  llvm::Value *value = builder->CreateLoad(termType, argPtr);

  values.insert({name, value});
}

void State::ret(int var) {
  llvm::Value *varValue = values[var];
  builder->CreateRet(varValue);
}

void State::retSymbol(int var) {
  llvm::Value *varValue = values[var];
  llvm::Value *symbol = builder->CreateExtractValue(varValue, 2);
  builder->CreateRet(symbol);
}

void State::write() {
  llvm::raw_string_ostream verifyOS(message);
  if (verifyModule(mod, &verifyOS)) {
    error = InvalidModule;
    return;
  }

  llvm::LoopAnalysisManager lam;
  llvm::FunctionAnalysisManager fam;
  llvm::CGSCCAnalysisManager cgam;
  llvm::ModuleAnalysisManager mam;
  llvm::PassBuilder pb;
  pb.registerModuleAnalyses(mam);
  pb.registerCGSCCAnalyses(cgam);
  pb.registerFunctionAnalyses(fam);
  pb.registerLoopAnalyses(lam);
  pb.crossRegisterProxies(lam, fam, cgam, mam);
  llvm::ModulePassManager mpm =
    pb.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
  mpm.run(mod, mam);

  std::error_code ec;
  llvm::raw_fd_ostream objectFile("main.o", ec, llvm::sys::fs::OF_None);
  if (ec) {
    message = ec.message();
    error = UnableToOpenObjectFile;
    return;
  }

  llvm::legacy::PassManager pass;
  bool emitPassOk = targetMachine->addPassesToEmitFile(
    pass, objectFile, nullptr, llvm::CodeGenFileType::ObjectFile
  );
  if (emitPassOk) {
    error = UnableToEmitObjectFile;
    return;
  }
  pass.run(mod);
  objectFile.close();

  int compilerExitCode = std::system("cc -o main main.o");
  if (compilerExitCode != 0) {
    std::stringstream stream;
    stream << "process exited with code ";
    stream << compilerExitCode;
    message = stream.str();
    error = LinkFailed;
    return;
  }
}

void State::print() { mod.print(llvm::outs(), nullptr); }

void State::printError() {
  static const char *info[] = {
    "No error",
    "Failed to look up target triple on your machine",
    "Internal compiler error: generated code failed validation",
    "Unable to write to object file",
    "Internal compiler error: object file not supported",
    "Failed to invoke the C compiler on your machine",
  };

  llvm::errs() << "error: " << info[error] << ": " << message << "\n";
}

bool State::ok() { return error == None; }
