#include "state.h"

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

State::State() : mod(initMod()), targetMachine(initTargetMachine()) {}

llvm::Module State::initMod() {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  return llvm::Module("output", context);
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

void State::generate() {
  llvm::FunctionType *mainFuncType =
    llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
  llvm::Function *mainFunc = llvm::Function::Create(
    mainFuncType, llvm::Function::ExternalLinkage, "main", mod
  );

  llvm::BasicBlock *entryBlock =
    llvm::BasicBlock::Create(context, "entry", mainFunc);
  llvm::IRBuilder builder(entryBlock);

  std::vector<llvm::Type *> printfArgsTypes = {
    llvm::PointerType::get(context, 0)
  };
  llvm::FunctionType *printfType = llvm::FunctionType::get(
    llvm::Type::getInt32Ty(context), printfArgsTypes, true
  );
  llvm::Function *printfFunc = llvm::Function::Create(
    printfType, llvm::Function::ExternalLinkage, "printf", mod
  );

  llvm::Constant *helloWorldStr =
    builder.CreateGlobalStringPtr("Hello, World!\n", "hello_world");

  std::vector<llvm::Value *> printfArgs = {helloWorldStr};
  builder.CreateCall(printfFunc, printfArgs, "printf_call");

  builder.CreateRet(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));
}

void State::output() {
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
  llvm::raw_fd_ostream outputFile("output.o", ec, llvm::sys::fs::OF_None);
  if (ec) {
    message = ec.message();
    error = UnableToOpenOutput;
    return;
  }

  llvm::legacy::PassManager pass;
  if (targetMachine->addPassesToEmitFile(
        pass, outputFile, nullptr, llvm::CodeGenFileType::ObjectFile
      )) {
    error = UnableToEmitObject;
    return;
  }
  pass.run(mod);
  outputFile.close();

  int result = std::system("cc -o output output.o");
  if (result != 0) {
    std::stringstream stream;
    stream << "process exited with code ";
    stream << result;
    message = stream.str();
    error = LinkFailed;
    return;
  }
}

void State::printError() {
  static const char *info[] = {
    "No error",
    "Failed to look up target triple on your machine",
    "Internal compiler error: generated code failed validation",
    "Unable to write to object file",
    "Internal compiler error: object file not supported",
    "Failed to invoke the C compiler on your machine",
  };

  std::cout << "error: " << info[error] << ": " << message << std::endl;
}
