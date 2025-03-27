#pragma once

#include <llvm/ExecutionEngine/Orc/LLJIT.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Linker/Linker.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>

#include <sstream>

template <typename Key> class State {
public:
  enum Error {
    None,
    NoTargetTriple,
    InvalidModule,
    UnableToOpenObjectFile,
    UnableToEmitObjectFile,
    RuntimeLoadFailed,
    RuntimeLinkFailed,
    SystemLinkFailed,
  };

  Error error = None;
  std::string message;

  State()
    : mod(initMod()),
      targetMachine(initTargetMachine()),
      termType(initTermType()),
      funType(initFunType()),
      noopFun(initNoopFun()),
      newAppFun(initNewAppFun()),
      newPartialFun(initNewPartialFun()),
      appPartialFun(initAppPartialFun()),
      copyFun(initCopyFun()),
      freeFun(initFreeFun()),
      freeTermFun(initFreeTermFun()) {}

  void main() {
    llvm::FunctionType *funType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(context), {}, false);
    fun = llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "main", *mod
    );

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
    builder.emplace(block);

    locals.clear();
  }

  void data(Key name, int symbol, int arity) {
    addGlobal(noopFun, name, symbol, arity);
  }

  void function(Key name, Key argName, int symbol, int arity) {
    fun =
      llvm::Function::Create(funType, llvm::Function::PrivateLinkage, "", *mod);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "entry", fun);
    builder.emplace(block);

    argument = fun->getArg(0);
    llvm::LoadInst *argLoad = builder->CreateLoad(termType, argument);
    llvm::AllocaInst *argAlloca = builder->CreateAlloca(termType, nullptr);
    builder->CreateStore(argLoad, argAlloca);
    locals.clear();
    locals.insert({argName, argAlloca});

    addGlobal(fun, name, symbol, arity);
  }

  void loadData(Key name, Key globalName) {
    llvm::GlobalVariable *global = globals[globalName];

    llvm::AllocaInst *termAlloca = builder->CreateAlloca(termType, nullptr);
    llvm::LoadInst *termLoad = builder->CreateLoad(termType, global);
    builder->CreateStore(termLoad, termAlloca);

    locals.insert({name, termAlloca});
  }

  void loadArg(Key name, Key var, int i) {
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

  void newApp(Key name, Key var, int length, Key *args) {
    callApp(newAppFun, name, var, length, args);
  }

  void newPartial(Key name, Key var, int length, Key *args) {
    callApp(newPartialFun, name, var, length, args);
  }

  void appPartial(Key name, Key var, int length, Key *args) {
    callApp(appPartialFun, name, var, length, args);
  }

  void copy(Key name, Key var) {
    llvm::AllocaInst *dest = builder->CreateAlloca(termType, nullptr);
    llvm::AllocaInst *src = locals[var];
    builder->CreateCall(copyFun, {dest, src});
    locals.insert({name, dest});
  }

  void freeArgs(Key var) {
    llvm::AllocaInst *term = locals[var];

    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    llvm::Value *argsField = builder->CreateExtractValue(termLoad, 1);
    builder->CreateCall(freeFun, {argsField});
  }

  void freeTerm(Key var) {
    llvm::AllocaInst *term = locals[var];
    builder->CreateCall(freeTermFun, {term});
  }

  void call(Key name, Key var) {
    llvm::AllocaInst *term = locals[var];

    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    llvm::Value *fun = builder->CreateExtractValue(termLoad, 0);
    llvm::AllocaInst *termAlloca = builder->CreateAlloca(termType, nullptr);
    builder->CreateStore(termLoad, termAlloca);
    builder->CreateCall(funType, fun, {termAlloca});

    locals.insert({name, termAlloca});
  }

  void returnTerm(Key var) {
    llvm::AllocaInst *term = locals[var];
    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    builder->CreateStore(termLoad, argument);
    builder->CreateRetVoid();
  }

  void returnSymbol(Key var) {
    llvm::AllocaInst *term = locals[var];
    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    llvm::Value *symbol = builder->CreateExtractValue(termLoad, 2);
    builder->CreateRet(symbol);
  }

  void match(Key var) {
    llvm::AllocaInst *term = locals[var];
    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    llvm::Value *symbol = builder->CreateExtractValue(termLoad, 2);

    llvm::BasicBlock *defaultArm =
      llvm::BasicBlock::Create(context, "default", fun);
    swit = builder->CreateSwitch(symbol, defaultArm, 0);
    builder->SetInsertPoint(defaultArm);
    builder->CreateUnreachable();
  }

  void arm(int symbol) {
    llvm::ConstantInt *symbolConstant =
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), symbol);
    llvm::BasicBlock *armBlock = llvm::BasicBlock::Create(context, "", fun);
    swit->addCase(symbolConstant, armBlock);
    builder->SetInsertPoint(armBlock);
  }

  void linkRuntime() {
    llvm::SMDiagnostic err;
    std::unique_ptr<llvm::Module> runtime =
      llvm::parseIRFile("rt.bc", err, context);
    if (!runtime) {
      error = RuntimeLoadFailed;
      message = err.getMessage();
      return;
    }

    bool failed = llvm::Linker::linkModules(*mod, std::move(runtime));
    if (failed) {
      error = RuntimeLinkFailed;
      return;
    }

    mod->getFunction("noop")->setLinkage(llvm::Function::PrivateLinkage);
    mod->getFunction("newApp")->setLinkage(llvm::Function::PrivateLinkage);
    mod->getFunction("newPartial")->setLinkage(llvm::Function::PrivateLinkage);
    mod->getFunction("appPartial")->setLinkage(llvm::Function::PrivateLinkage);
    mod->getFunction("copy")->setLinkage(llvm::Function::PrivateLinkage);
    mod->getFunction("freeTerm")->setLinkage(llvm::Function::PrivateLinkage);
  }

  void validate() {
    llvm::raw_string_ostream verifyOS(message);
    if (verifyModule(*mod, &verifyOS)) {
      error = InvalidModule;
      return;
    }
  }

  void optimize() {
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
    mpm.run(*mod, mam);
  }

  void writeObjectFile() {
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
    pass.run(*mod);
    objectFile.close();
  }

  void linkObjectFile() {
    int compilerExitCode = std::system("cc -o main main.o");
    if (compilerExitCode != 0) {
      std::stringstream stream;
      stream << "process exited with code ";
      stream << compilerExitCode;
      message = stream.str();
      error = SystemLinkFailed;
      return;
    }
  }

  int32_t jit() {
    llvm::ExitOnError exitOnError;

    std::unique_ptr<llvm::orc::LLJIT> jit =
      exitOnError(llvm::orc::LLJITBuilder().create());

    exitOnError(jit->addIRModule(llvm::orc::ThreadSafeModule(
      std::move(mod), std::make_unique<llvm::LLVMContext>()
    )));

    llvm::orc::ExecutorAddr mainAddr = exitOnError(jit->lookup("main"));
    using MainType = int32_t (*)();
    MainType mainFun = mainAddr.toPtr<MainType>();
    int32_t result = mainFun();
    return result;
  }

  void print() { mod->print(llvm::outs(), nullptr); }

  void printError() {
    static const char *info[] = {
      "No error",
      "Failed to look up target triple on your machine",
      "Internal compiler error: generated code failed validation",
      "Unable to write to object file",
      "Internal compiler error: object file not supported",
      "Internal compiler error: failed to load runtime code",
      "Internal compiler error: failed to link runtime code",
      "Failed to invoke the C compiler on your machine",
    };

    llvm::errs() << "error: " << info[error] << ": " << message << "\n";
  }

  bool ok() { return error == None; }

private:
  llvm::LLVMContext context;
  std::unique_ptr<llvm::Module> mod;
  std::unique_ptr<llvm::TargetMachine> targetMachine;

  llvm::StructType *termType = nullptr;
  llvm::FunctionType *funType = nullptr;
  llvm::Function *noopFun = nullptr;
  llvm::Function *newAppFun = nullptr;
  llvm::Function *newPartialFun = nullptr;
  llvm::Function *appPartialFun = nullptr;
  llvm::Function *copyFun = nullptr;
  llvm::Function *freeFun = nullptr;
  llvm::Function *freeTermFun = nullptr;

  std::optional<llvm::IRBuilder<>> builder;
  std::unordered_map<Key, llvm::GlobalVariable *> globals;
  llvm::Function *fun = nullptr;
  llvm::Argument *argument = nullptr;
  std::unordered_map<Key, llvm::AllocaInst *> locals;
  llvm::SwitchInst *swit = nullptr;

  std::unique_ptr<llvm::Module> initMod() {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();

    return std::make_unique<llvm::Module>("main", context);
  }

  std::unique_ptr<llvm::TargetMachine> initTargetMachine() {
    std::string targetTriple = llvm::sys::getDefaultTargetTriple();
    mod->setTargetTriple(targetTriple);

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

    mod->setDataLayout(targetMachine->createDataLayout());

    return targetMachine;
  }

  llvm::StructType *initTermType() {
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

  llvm::FunctionType *initFunType() {
    return llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0)},
      false
    );
  }

  llvm::Function *initNoopFun() {
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "noop", *mod
    );
  }

  llvm::Function *initNewAppFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0),
       llvm::Type::getInt64Ty(context),
       llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "newApp", *mod
    );
  }

  llvm::Function *initNewPartialFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0),
       llvm::Type::getInt64Ty(context),
       llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "newPartial", *mod
    );
  }

  llvm::Function *initAppPartialFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0),
       llvm::Type::getInt64Ty(context),
       llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "appPartial", *mod
    );
  }

  llvm::Function *initCopyFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0), llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "copy", *mod
    );
  }

  llvm::Function *initFreeFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "free", *mod
    );
  }

  llvm::Function *initFreeTermFun() {
    llvm::FunctionType *funType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context),
      {llvm::PointerType::get(context, 0)},
      false
    );
    return llvm::Function::Create(
      funType, llvm::Function::ExternalLinkage, "freeTerm", *mod
    );
  }

  void callApp(llvm::Function *fun, Key name, Key var, int length, Key *args) {
    llvm::AllocaInst *term = locals[var];

    llvm::LoadInst *termLoad = builder->CreateLoad(termType, term);
    llvm::AllocaInst *termAlloca = builder->CreateAlloca(termType, nullptr);
    builder->CreateStore(termLoad, termAlloca);

    llvm::ConstantInt *lengthConstant =
      llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), length);

    llvm::ArrayType *argsType = llvm::ArrayType::get(termType, length);
    llvm::AllocaInst *argsAlloca = builder->CreateAlloca(argsType, nullptr);
    for (int i = 0; i < length; ++i) {
      Key arg = args[i];
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
    builder->CreateCall(fun, argValues);

    locals.insert({name, termAlloca});
  }

  void addGlobal(llvm::Function *fun, Key name, int symbol, int arity) {
    std::vector<llvm::Constant *> fieldValues = {
      fun, // fun
      llvm::ConstantPointerNull::get(llvm::PointerType::get(context, 0)
      ),                                                               // args
      llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), symbol), // symbol
      llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity),  // length
      llvm::ConstantInt::get(llvm::Type::getInt16Ty(context), arity) // capacity
    };
    llvm::Constant *termInit = llvm::ConstantStruct::get(termType, fieldValues);

    auto *global = new llvm::GlobalVariable(
      *mod,                              // Module
      termType,                          // Type
      true,                              // isConstant
      llvm::GlobalValue::PrivateLinkage, // Linkage
      termInit,                          // Initializer
      "",                                // Name
      nullptr,                           // InsertBefore
      llvm::GlobalValue::NotThreadLocal, // ThreadLocalMode
      0,                                 // AddressSpace
      false                              // isExternallyInitialized
    );

    globals.insert({name, global});
  }
};

using StateInt = State<int>;
