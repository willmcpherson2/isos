#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

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

class State {
public:
  State();

  void main();
  void data(int symbol, int arity);
  void function(int symbol, int arity);
  void loadData(int name, int symbol);
  void loadArg(int name, int var, int i);
  void copy(int name, int var);
  void call(int name, int var);
  void returnTerm(int var);
  void returnSymbol(int var);
  void freeArgs(int var);
  void freeTerm(int var);
  void newApp(int name, int var, int length, int *args);
  void newPartial(int name, int var, int length, int *args);
  void appPartial(int name, int var, int length, int *args);
  void match(int var);
  void arm(int symbol);

  void linkRuntime();
  void validate();
  void optimize();
  void writeObjectFile();
  void linkObjectFile();
  void print();
  void printError();
  bool ok();

  Error error = None;
  std::string message;

private:
  llvm::Module initMod();
  std::unique_ptr<llvm::TargetMachine> initTargetMachine();
  llvm::StructType *initTermType();
  llvm::FunctionType *initFunType();
  llvm::Function *initNoopFun();
  llvm::Function *initCopyFun();
  llvm::Function *initFreeFun();
  llvm::Function *initFreeTermFun();
  llvm::Function *initNewAppFun();
  llvm::Function *initNewPartialFun();
  llvm::Function *initAppPartialFun();

  void addGlobal(std::string name, llvm::Function *fun, int symbol, int arity);

  llvm::LLVMContext context;
  llvm::Module mod;
  std::unique_ptr<llvm::TargetMachine> targetMachine;
  llvm::StructType *termType = nullptr;
  llvm::FunctionType *funType = nullptr;
  llvm::Function *noopFun = nullptr;
  llvm::Function *copyFun = nullptr;
  llvm::Function *freeFun = nullptr;
  llvm::Function *freeTermFun = nullptr;
  llvm::Function *newAppFun = nullptr;
  llvm::Function *newPartialFun = nullptr;
  llvm::Function *appPartialFun = nullptr;
  std::optional<llvm::IRBuilder<>> builder;
  std::unordered_map<int, llvm::GlobalVariable *> globals;
  llvm::Function *fun = nullptr;
  llvm::Argument *argument = nullptr;
  std::unordered_map<int, llvm::AllocaInst *> locals;
  llvm::SwitchInst *swit = nullptr;
};
