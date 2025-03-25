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
  void newApp(int name, int var, int length, int *args);
  void newPartial(int name, int var, int length, int *args);
  void appPartial(int name, int var, int length, int *args);
  void copy(int name, int var);
  void freeArgs(int var);
  void freeTerm(int var);
  void call(int name, int var);
  void returnTerm(int var);
  void returnSymbol(int var);
  void match(int var);
  void arm(int symbol);

  void linkRuntime();
  void validate();
  void optimize();
  void writeObjectFile();
  void linkObjectFile();
  int32_t jit();
  void print();
  void printError();
  bool ok();

  Error error = None;
  std::string message;

private:
  std::unique_ptr<llvm::Module> initMod();
  std::unique_ptr<llvm::TargetMachine> initTargetMachine();
  llvm::StructType *initTermType();
  llvm::FunctionType *initFunType();
  llvm::Function *initNoopFun();
  llvm::Function *initNewAppFun();
  llvm::Function *initNewPartialFun();
  llvm::Function *initAppPartialFun();
  llvm::Function *initCopyFun();
  llvm::Function *initFreeFun();
  llvm::Function *initFreeTermFun();

  void callApp(llvm::Function *fun, int name, int var, int length, int *args);
  void addGlobal(std::string name, llvm::Function *fun, int symbol, int arity);

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
  std::unordered_map<int, llvm::GlobalVariable *> globals;
  llvm::Function *fun = nullptr;
  llvm::Argument *argument = nullptr;
  std::unordered_map<int, llvm::AllocaInst *> locals;
  llvm::SwitchInst *swit = nullptr;
};
