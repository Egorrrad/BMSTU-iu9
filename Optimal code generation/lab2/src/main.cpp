#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

int main() {
    LLVMContext context;
    Module module("simple_compiler", context);

    IRBuilder<> builder(context);

    FunctionType *funcType = FunctionType::get(builder.getInt32Ty(), false);
    Function *mainFunc = Function::Create(funcType, Function::ExternalLinkage, "main", &module);
    BasicBlock *entry = BasicBlock::Create(context, "entry", mainFunc);
    builder.SetInsertPoint(entry);

    Value *sum = builder.CreateAdd(
        builder.getInt32(353),
        builder.getInt32(48)
    );
    builder.CreateRet(sum);

    module.print(outs(), nullptr);
    return 0;
}