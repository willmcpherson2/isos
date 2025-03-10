#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

enum Error {
  None,
  NoTargetTriple,
  InvalidModule,
  UnableToOpenObjectFile,
  UnableToEmitObjectFile,
  LinkFailed,
};

class State {
public:
  State();
  void generate();
  void output();
  void printError();

  Error error = None;
  std::string message;

private:
  llvm::Module initMod();
  std::unique_ptr<llvm::TargetMachine> initTargetMachine();

  llvm::LLVMContext context;
  llvm::Module mod;
  std::unique_ptr<llvm::TargetMachine> targetMachine;
};
