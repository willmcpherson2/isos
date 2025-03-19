#include "state.h"

void State::main() {
  auto name = "main";

  llvm::FunctionType *funType =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {}, false);
  llvm::Function *fun =
    llvm::Function::Create(funType, llvm::Function::ExternalLinkage, name, mod);

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  builder.emplace(block);

  locals.clear();
}

void State::data(int symbol, int arity) {
  auto name = "data_" + std::to_string(symbol);
  addGlobal(name, noopFun, symbol, arity);
}

void State::fun(int symbol, int arity) {
  auto funName = "fun_" + std::to_string(symbol);

  llvm::Function *fun = llvm::Function::Create(
    funType, llvm::Function::PrivateLinkage, funName, mod
  );

  llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
  builder.emplace(block);

  llvm::Argument *arg = fun->getArg(0);
  llvm::AllocaInst *argAlloca = builder->CreateAlloca(termType, nullptr);
  builder->CreateStore(arg, argAlloca);
  locals.clear();
  locals.insert({0, argAlloca});

  auto name = "data_" + std::to_string(symbol);
  addGlobal(name, fun, symbol, arity);
}

void State::loadData(int name, int symbol) {
  llvm::GlobalVariable *global = globals[symbol];

  llvm::AllocaInst *termAlloca = builder->CreateAlloca(termType, nullptr);
  llvm::LoadInst *termLoad = builder->CreateLoad(termType, global);
  builder->CreateStore(termLoad, termAlloca);

  locals.insert({name, termAlloca});
}

void State::loadArg(int name, int var, int i) {
  llvm::AllocaInst *term = locals[var];

  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  llvm::Value *argsField = builder->CreateExtractValue(termLoad, 1);
  llvm::ConstantInt *argIndex =
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i);
  llvm::Value *argPtr = builder->CreateGEP(termType, argsField, argIndex);
  llvm::LoadInst *arg = builder->CreateLoad(termType, argPtr);
  llvm::AllocaInst *argAlloca = builder->CreateAlloca(termType, nullptr);
  builder->CreateStore(arg, argAlloca);

  locals.insert({name, argAlloca});
}

void State::call(int name, int var) {
  llvm::AllocaInst *term = locals[var];

  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  llvm::Value *fun = builder->CreateExtractValue(termLoad, 0);
  llvm::CallInst *result = builder->CreateCall(funType, fun, {termLoad});
  llvm::AllocaInst *resultAlloca = builder->CreateAlloca(termType, nullptr);
  builder->CreateStore(result, resultAlloca);

  locals.insert({name, resultAlloca});
}

void State::returnTerm(int var) {
  llvm::AllocaInst *term = locals[var];
  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  builder->CreateRet(termLoad);
}

void State::returnSymbol(int var) {
  llvm::AllocaInst *term = locals[var];
  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  llvm::Value *symbol = builder->CreateExtractValue(termLoad, 2);
  builder->CreateRet(symbol);
}

void State::freeArgs(int var) {
  llvm::AllocaInst *term = locals[var];

  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  llvm::Value *argsField = builder->CreateExtractValue(termLoad, 1);
  builder->CreateCall(freeFun, {argsField});
}

void State::appNew(int name, int var, int length, int *args) {
  llvm::AllocaInst *term = locals[var];

  llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
  llvm::AllocaInst *termAlloca = builder->CreateAlloca(termType, nullptr);
  builder->CreateStore(termLoad, termAlloca);

  llvm::ConstantInt *lengthConstant =
    llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), length);

  llvm::ArrayType *argsType = llvm::ArrayType::get(termType, length);
  llvm::AllocaInst *argsAlloca = builder->CreateAlloca(argsType, nullptr);
  for (int i = 0; i < length; ++i) {
    int arg = args[i];
    llvm::AllocaInst *argLocal = locals[arg];
    llvm::LoadInst *argLoad = builder->CreateLoad(termType, argLocal);
    llvm::Value *argGep = builder->CreateGEP(
      argsType,
      argsAlloca,
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i)}
    );
    builder->CreateStore(argLoad, argGep);
  }

  std::vector<llvm::Value *> argValues;
  argValues.push_back(termAlloca);
  argValues.push_back(lengthConstant);
  argValues.push_back(argsAlloca);
  builder->CreateCall(appNewFun, argValues);

  locals.insert({name, termAlloca});
}

void State::addGlobal(
  std::string name,
  llvm::Function *fun,
  int symbol,
  int arity
) {
  std::vector<llvm::Constant *> fieldValues = {
    fun,                                                                // fun
    llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0)), // args
    llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), symbol), // symbol
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity),  // length
    llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity)   // capacity
  };
  llvm::Constant *termInit = llvm::ConstantStruct::get(termType, fieldValues);

  auto *global = new llvm::GlobalVariable(
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
