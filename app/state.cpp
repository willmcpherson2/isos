#include "state.h"

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Linker/Linker.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/TargetParser/Host.h>

#include <sstream>

State::State()
  : mod(initMod()),
    targetMachine(initTargetMachine()),
    termType(initTermType()),
    funType(initFunType()),
    noopFun(initNoopFun()),
    freeFun(initFreeFun()),
    freeTermFun(initFreeTermFun()),
    newAppFun(initNewAppFun()),
    newPartialFun(initNewPartialFun()),
    appPartialFun(initAppPartialFun()),
    copyFun(initCopyFun()) {}

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
  return llvm::FunctionType::get(
    llvm::Type::getVoidTy(context), {llvm::PointerType::get(context, 0)}, false
  );
}

llvm::Function *State::initNoopFun() {
  auto name = "noop";

  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

llvm::Function *State::initCopyFun() {
  auto name = "copy";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context),
    {llvm::PointerType::get(context, 0), llvm::PointerType::get(context, 0)},
    false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
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

llvm::Function *State::initFreeTermFun() {
  auto name = "free_term";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context), {llvm::PointerType::get(context, 0)}, false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

llvm::Function *State::initNewAppFun() {
  auto name = "new_app";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context),
    {llvm::PointerType::get(context, 0),
     llvm::Type::getInt64Ty(context),
     llvm::PointerType::get(context, 0)},
    false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

llvm::Function *State::initNewPartialFun() {
  auto name = "new_partial";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context),
    {llvm::PointerType::get(context, 0),
     llvm::Type::getInt64Ty(context),
     llvm::PointerType::get(context, 0)},
    false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

llvm::Function *State::initAppPartialFun() {
  auto name = "app_partial";

  llvm::FunctionType *funType = llvm::FunctionType::get(
    llvm::Type::getVoidTy(context),
    {llvm::PointerType::get(context, 0),
     llvm::Type::getInt64Ty(context),
     llvm::PointerType::get(context, 0)},
    false
  );
  return llvm::Function::Create(
    funType, llvm::Function::ExternalLinkage, name, mod
  );
}

void State::linkRuntime() {
  llvm::SMDiagnostic err;
  std::unique_ptr<llvm::Module> runtime =
    llvm::parseIRFile("rt.bc", err, context);
  if (!runtime) {
    error = RuntimeLoadFailed;
    message = err.getMessage();
    return;
  }

  bool failed = llvm::Linker::linkModules(mod, std::move(runtime));
  if (failed) {
    error = RuntimeLinkFailed;
    return;
  }

  mod.getFunction("noop")->setLinkage(llvm::Function::PrivateLinkage);
  mod.getFunction("new_app")->setLinkage(llvm::Function::PrivateLinkage);
  mod.getFunction("new_partial")->setLinkage(llvm::Function::PrivateLinkage);
  mod.getFunction("app_partial")->setLinkage(llvm::Function::PrivateLinkage);
  mod.getFunction("copy")->setLinkage(llvm::Function::PrivateLinkage);
  mod.getFunction("free_term")->setLinkage(llvm::Function::PrivateLinkage);
}

void State::validate() {
  llvm::raw_string_ostream verifyOS(message);
  if (verifyModule(mod, &verifyOS)) {
    error = InvalidModule;
    return;
  }
}

void State::optimize() {
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
}

void State::writeObjectFile() {
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
}

void State::linkObjectFile() {
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

void State::print() { mod.print(llvm::outs(), nullptr); }

void State::printError() {
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

bool State::ok() { return error == None; }
