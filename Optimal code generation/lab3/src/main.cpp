#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Constants.h>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

// Токены языка
enum Token {
    TOK_EOF = -1,      
    TOK_IDENT = -2,     
    TOK_NUM = -3,       
    TOK_IF = -4,        
    TOK_ELSE = -5,      
    TOK_WHILE = -6,     
    TOK_RETURN = -7,    
    TOK_FOR = -8,       
    TOK_LBRACE = -9,    
    TOK_RBRACE = -10
};


static std::string IdentifierStr;
static int NumVal;              
static int CurTok;               
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName);

// Лексический анализатор
static int getNextToken() {
    static int LastChar = ' ';
    
    // Пропускаем пробельные символы
    while (isspace(LastChar))
        LastChar = getchar();
    
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;
            
        if (IdentifierStr == "if") return TOK_IF;
        if (IdentifierStr == "else") return TOK_ELSE;
        if (IdentifierStr == "while") return TOK_WHILE;
        if (IdentifierStr == "for") return TOK_FOR;
        if (IdentifierStr == "return") return TOK_RETURN;
        return TOK_IDENT;  // Не ключевое слово - значит идентификатор
    }
    
    if (isdigit(LastChar)) {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar));
        
        NumVal = strtol(NumStr.c_str(), nullptr, 10);
        return TOK_NUM;
    }
    
    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        
        if (LastChar != EOF)
            return getNextToken();
    }
    
    if (LastChar == EOF)
        return TOK_EOF;
    
    if (LastChar == '{') {
        LastChar = getchar();
        return TOK_LBRACE;
    }
    
    if (LastChar == '}') {
        LastChar = getchar();
        return TOK_RBRACE;
    }
    
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

class ExprAST {
public:
    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;
};

// Класс для числовых литералов
class NumberExprAST : public ExprAST {
    int Val;
public:
    NumberExprAST(int Val) : Val(Val) {}
    Value *codegen() override;
};

// Класс для переменных
class VariableExprAST : public ExprAST {
    std::string Name;
public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value *codegen() override;
};

// Класс для бинарных операций
class BinaryExprAST : public ExprAST {
    char Op;  // Оператор (+, -, *, <, >)
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                 std::unique_ptr<ExprAST> RHS)
        : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value *codegen() override;
};

// Класс для блоков кода
class BlockExprAST : public ExprAST {
    std::vector<std::unique_ptr<ExprAST>> Exprs;
public:
    BlockExprAST(std::vector<std::unique_ptr<ExprAST>> Exprs)
        : Exprs(std::move(Exprs)) {}
    Value *codegen() override;
};

// Класс для условного оператора if
class IfExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Cond;
    std::unique_ptr<ExprAST> Then; 
    std::unique_ptr<ExprAST> Else;
public:
    IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
              std::unique_ptr<ExprAST> Else)
        : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
    Value *codegen() override;
};

// Класс для цикла for
class ForExprAST : public ExprAST {
    std::string VarName;        
    std::unique_ptr<ExprAST> Start, End, Step, Body;  
public:
    ForExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
               std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
               std::unique_ptr<ExprAST> Body)
        : VarName(VarName), Start(std::move(Start)), End(std::move(End)),
          Step(std::move(Step)), Body(std::move(Body)) {}
    Value *codegen() override;
};

// Класс для присваивания
class AssignExprAST : public ExprAST {
    std::string Name;           
    std::unique_ptr<ExprAST> Expr;  
public:
    AssignExprAST(const std::string &Name, std::unique_ptr<ExprAST> Expr)
        : Name(Name), Expr(std::move(Expr)) {}
    Value *codegen() override;
};

// Класс для оператора return
class ReturnExprAST : public ExprAST {
    std::unique_ptr<ExprAST> Expr;  
public:
    ReturnExprAST(std::unique_ptr<ExprAST> Expr) : Expr(std::move(Expr)) {}
    Value *codegen() override;
};

static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<ExprAST> ParseBlock();

// Парсер числового литерала
static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);  
    CurTok = getNextToken();  
    return std::move(Result);
}

// Парсер выражения в скобках
static std::unique_ptr<ExprAST> ParseParenExpr() {
    CurTok = getNextToken();  
    auto V = ParseExpression(); 
    if (!V)
        return nullptr;
    
    if (CurTok != ')') {
        fprintf(stderr, "expected ')'\n");
        return nullptr;
    }
    CurTok = getNextToken();
    return V;
}

// Парсер идентификатора
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;
    
    CurTok = getNextToken();
    
    if (CurTok != '=')
        return std::make_unique<VariableExprAST>(IdName);
    
    CurTok = getNextToken();
    auto Expr = ParseExpression();
    if (!Expr)
        return nullptr;
    
    return std::make_unique<AssignExprAST>(IdName, std::move(Expr));
}

// Парсер блока кода
static std::unique_ptr<ExprAST> ParseBlock() {
    CurTok = getNextToken();
    
    std::vector<std::unique_ptr<ExprAST>> Exprs;
    while (CurTok != TOK_RBRACE && CurTok != TOK_EOF) {
        auto Expr = ParseExpression();
        if (!Expr)
            return nullptr;
        Exprs.push_back(std::move(Expr));
        
        if (CurTok == ';') {
            CurTok = getNextToken();
        }
    }
    
    if (CurTok != TOK_RBRACE) {
        fprintf(stderr, "expected '}'\n");
        return nullptr;
    }
    
    CurTok = getNextToken();
    return std::make_unique<BlockExprAST>(std::move(Exprs));
}

// Парсер условного оператора if
static std::unique_ptr<ExprAST> ParseIfExpr() {
    CurTok = getNextToken();
    
    if (CurTok != '(') {
        fprintf(stderr, "expected '(' after 'if'\n");
        return nullptr;
    }
    
    CurTok = getNextToken();
    auto Cond = ParseExpression();
    if (!Cond)
        return nullptr;
    
    if (CurTok != ')') {
        fprintf(stderr, "expected ')' after if condition\n");
        return nullptr;
    }
    CurTok = getNextToken();
    
    if (CurTok != TOK_LBRACE) {
        fprintf(stderr, "expected '{' after if condition\n");
        return nullptr;
    }
    
    auto Then = ParseBlock();
    if (!Then)
        return nullptr;
    
    std::unique_ptr<ExprAST> Else = nullptr;
    if (CurTok == TOK_ELSE) {
        CurTok = getNextToken();
        
        if (CurTok != TOK_LBRACE) {
            fprintf(stderr, "expected '{' after else\n");
            return nullptr;
        }
        
        Else = ParseBlock();
        if (!Else)
            return nullptr;
    }
    return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

// Парсер цикла for
static std::unique_ptr<ExprAST> ParseForExpr() {
    CurTok = getNextToken();
    
    if (CurTok != '(') {
        fprintf(stderr, "expected '(' after 'for'\n");
        return nullptr;
    }
    CurTok = getNextToken();
    
    if (CurTok != TOK_IDENT) {
        fprintf(stderr, "expected identifier after 'for('\n");
        return nullptr;
    }
    std::string IdName = IdentifierStr;
    CurTok = getNextToken();
    
    if (CurTok != '=') {
        fprintf(stderr, "expected '=' after for variable\n");
        return nullptr;
    }
    CurTok = getNextToken();
    
    auto Start = ParseExpression();
    if (!Start)
        return nullptr;
    
    if (CurTok != ';') {
        fprintf(stderr, "expected ';' after for start value\n");
        return nullptr;
    }
    CurTok = getNextToken();
    
    auto End = ParseExpression();
    if (!End)
        return nullptr;
    
    std::unique_ptr<ExprAST> Step = nullptr;
    if (CurTok == ';') {
        CurTok = getNextToken();
        Step = ParseExpression();
        if (!Step)
            return nullptr;
    }
    
    if (CurTok != ')') {
        fprintf(stderr, "expected ')' after for clauses\n");
        return nullptr;
    }
    CurTok = getNextToken();
    
    if (CurTok != TOK_LBRACE) {
        fprintf(stderr, "expected '{' after for\n");
        return nullptr;
    }
    
    auto Body = ParseBlock();
    if (!Body)
        return nullptr;
    
    return std::make_unique<ForExprAST>(IdName, std::move(Start), std::move(End),
                                std::move(Step), std::move(Body));
}

// Парсер оператора return
static std::unique_ptr<ExprAST> ParseReturnExpr() {
    CurTok = getNextToken();
    auto Expr = ParseExpression();
    if (!Expr)
        return nullptr;
    return std::make_unique<ReturnExprAST>(std::move(Expr));
}

// Парсер выражений 
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        case TOK_IDENT: return ParseIdentifierExpr();
        case TOK_NUM:   return ParseNumberExpr();
        case '(':       return ParseParenExpr();
        case TOK_IF:    return ParseIfExpr();
        case TOK_FOR:   return ParseForExpr();
        case TOK_RETURN: return ParseReturnExpr();
        case TOK_LBRACE: return ParseBlock();
        default:
            fprintf(stderr, "unknown token when expecting an expression: %d\n", CurTok);
            return nullptr;
    }
}

static int GetTokPrecedence() {
    switch (CurTok) {
        case '<':
        case '>': return 10;
        case '+':
        case '-': return 20;
        case '*': return 40;   
        default: return -1;
    }
}

// Парсер бинарных операций
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS) {
    while (true) {
        int TokPrec = GetTokPrecedence();
        
        if (TokPrec < ExprPrec)
            return LHS;
        
        int BinOp = CurTok; 
        CurTok = getNextToken();
        
        auto RHS = ParsePrimary();
        if (!RHS)
            return nullptr;
        
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }
        
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;
    
    return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<IRBuilder<>> Builder;
static std::map<std::string, AllocaInst*> NamedValues;

// Генерация кода для числового литерала
Value *NumberExprAST::codegen() {
    return ConstantInt::get(*TheContext, APInt(32, Val, true));
}

// Генерация кода для переменной
Value *VariableExprAST::codegen() {
    AllocaInst *A = NamedValues[Name];
    if (!A) {
        fprintf(stderr, "unknown variable name: %s\n", Name.c_str());
        return nullptr;
    }
    return Builder->CreateLoad(Type::getInt32Ty(*TheContext), A, Name.c_str());
}

// Генерация кода для бинарной операции
Value *BinaryExprAST::codegen() {
    
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;
    
    switch (Op) {
        case '+': return Builder->CreateAdd(L, R, "addtmp");
        case '-': return Builder->CreateSub(L, R, "subtmp");
        case '*': return Builder->CreateMul(L, R, "multmp");
        case '<':
            L = Builder->CreateICmpSLT(L, R, "cmptmp");
            return Builder->CreateZExt(L, Type::getInt32Ty(*TheContext), "booltmp");
        case '>':
            L = Builder->CreateICmpSGT(L, R, "cmptmp");
            return Builder->CreateZExt(L, Type::getInt32Ty(*TheContext), "booltmp");
        default:
            fprintf(stderr, "invalid binary operator\n");
            return nullptr;
    }
}

// Генерация кода для блока
Value *BlockExprAST::codegen() {
    Value *Last = nullptr;
    for (auto &Expr : Exprs) {
        Last = Expr->codegen();
        if (!Last)
            return nullptr;
    }
    return Last;
}

// Генерация кода для if
Value *IfExprAST::codegen() {
    Value *CondV = Cond->codegen();
    if (!CondV)
        return nullptr;
    
    CondV = Builder->CreateICmpNE(
        CondV, ConstantInt::get(*TheContext, APInt(32, 0, true)), "ifcond");
    
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    
    BasicBlock *ThenBB = BasicBlock::Create(*TheContext, "then", TheFunction);  
    BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else");              
    BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont");          
    
    Builder->CreateCondBr(CondV, ThenBB, ElseBB);
    
    AllocaInst *ResultVar = CreateEntryBlockAlloca(TheFunction, "if.result");
    
    Builder->SetInsertPoint(ThenBB);
    Value *ThenV = Then->codegen();
    if (!ThenV)
        return nullptr;

    Builder->CreateStore(ThenV, ResultVar);
    Builder->CreateBr(MergeBB);
    
    TheFunction->insert(TheFunction->end(), ElseBB);
    Builder->SetInsertPoint(ElseBB);
    Value *ElseV = Else ? Else->codegen() : ConstantInt::get(*TheContext, APInt(32, 0, true));
    if (!ElseV)
        return nullptr;

    Builder->CreateStore(ElseV, ResultVar);
    Builder->CreateBr(MergeBB);
    
    TheFunction->insert(TheFunction->end(), MergeBB);
    Builder->SetInsertPoint(MergeBB);
    
    return Builder->CreateLoad(Type::getInt32Ty(*TheContext), ResultVar, "if.result");
}

// Генерация кода для цикла for
Value *ForExprAST::codegen() {
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    
    AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    
    Value *StartVal = Start->codegen();
    if (!StartVal)
        return nullptr;
    Builder->CreateStore(StartVal, Alloca);
    
    BasicBlock *LoopBB = BasicBlock::Create(*TheContext, "loop", TheFunction);
    Builder->CreateBr(LoopBB);
    Builder->SetInsertPoint(LoopBB);
    
    NamedValues[VarName] = Alloca;
    
    Value *BodyVal = Body->codegen();
    if (!BodyVal)
        return nullptr;
    
    Value *StepVal = nullptr;
    if (Step) {
        StepVal = Step->codegen();
        if (!StepVal)
            return nullptr;
    } else {
        StepVal = ConstantInt::get(*TheContext, APInt(32, 1, true));
    }
    
    Value *CurVar = Builder->CreateLoad(Type::getInt32Ty(*TheContext), Alloca, VarName.c_str());
    Value *NextVar = Builder->CreateAdd(CurVar, StepVal, "nextvar");
    Builder->CreateStore(NextVar, Alloca);
    
    Value *EndCond = End->codegen();
    if (!EndCond)
        return nullptr;
    
    EndCond = Builder->CreateICmpNE(
        EndCond, ConstantInt::get(*TheContext, APInt(32, 0, true)), "loopcond");
    
    BasicBlock *AfterBB = BasicBlock::Create(*TheContext, "afterloop", TheFunction);
    Builder->CreateCondBr(EndCond, LoopBB, AfterBB);
    Builder->SetInsertPoint(AfterBB);
    
    NamedValues.erase(VarName);
    return Constant::getNullValue(Type::getInt32Ty(*TheContext));
}

// Создание аллокации переменной в стеке функции
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) {
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type::getInt32Ty(*TheContext), nullptr, VarName);
}

// Генерация кода для присваивания
Value *AssignExprAST::codegen() {
    Value *Val = Expr->codegen();
    if (!Val)
        return nullptr;
    
    Function *TheFunction = Builder->GetInsertBlock()->getParent();
    AllocaInst *Alloca = NamedValues[Name];
    
    if (!Alloca) {
        Alloca = CreateEntryBlockAlloca(TheFunction, Name);
        NamedValues[Name] = Alloca;
    }
    
    Builder->CreateStore(Val, Alloca);
    return Val;
}

// Генерация кода для return
Value *ReturnExprAST::codegen() {
    Value *RetVal = Expr->codegen();
    if (!RetVal)
        return nullptr;

    Builder->CreateRet(RetVal);
    return RetVal;
}

int main() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("main", *TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
    
    // Создаем функцию main
    FunctionType *FT = FunctionType::get(Type::getInt32Ty(*TheContext), false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, "main", *TheModule);
    
    // Создаем начальный базовый блок
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", F);
    Builder->SetInsertPoint(BB);
    
    CurTok = getNextToken();
    while (CurTok != TOK_EOF) {
        auto Expr = ParseExpression();
        if (Expr) {
            Expr->codegen();
        } else {
            CurTok = getNextToken();
        }
        
        if (CurTok == ';') {
            CurTok = getNextToken();
        }
    }
    
    if (!Builder->GetInsertBlock()->getTerminator()) {
        Builder->CreateRet(ConstantInt::get(*TheContext, APInt(32, 0, true)));
    }
    
    TheModule->print(outs(), nullptr);
    
    return 0;
}