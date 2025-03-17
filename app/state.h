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
  LinkFailed,
};

class State {
public:
  State();

  void main();
  void data(int symbol, int arity);
  void function(int symbol, int arity);
  void load(int name, int symbol);
  void index(int name, int var, int i);
  void copy(int name, int var);
  void call(int name, int var);
  void ret(int var);
  void retSymbol(int var);
  void free(int var);
  void freeDeep(int var);
  void appNew(int name, int var, int length, int *args);
  void appFrom(int name, int old, int var, int length, int *args);
  void partialNew(int name, int var, int length, int *args);
  void partialFrom(int name, int old, int var, int length, int *args);
  void match(int var);
  void arm(int symbol);

  void write();
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
  llvm::Function *initFreeFun();
  llvm::Function *initAppNewFun();

  llvm::LLVMContext context;
  llvm::Module mod;
  std::unique_ptr<llvm::TargetMachine> targetMachine;
  llvm::StructType *termType = nullptr;
  llvm::FunctionType *funType = nullptr;
  llvm::Function *noopFun = nullptr;
  llvm::Function *freeFun = nullptr;
  llvm::Function *appNewFun = nullptr;
  std::optional<llvm::IRBuilder<>> builder;
  std::unordered_map<int, llvm::GlobalVariable *> globals;
  std::unordered_map<int, llvm::AllocaInst *> locals;
};
