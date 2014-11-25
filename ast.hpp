#include <vector>
#include <string>
#include <map>
#include "llvm/PassManager.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/TargetSelect.h"
using llvm::Value;
using llvm::Function;

// ExprAST - Base class for all expression nodes
class ExprAST {
public:
	virtual ~ExprAST() {}
	virtual Value *Codegen() = 0;
};

// NumberExprAST - Expression class for numeric literals like "1.0"
class NumberExprAST : public ExprAST {
	double Val;
public:
	NumberExprAST(double val) : Val(val) {}
	virtual Value *Codegen();
};

// For referencing a variable, like "a"
class VariableExprAST : public ExprAST {
	std::string Name;
public:
	VariableExprAST(const std::string &name) : Name(name) {}
	virtual Value *Codegen();
	const std::string& getName() const { return Name; }
};

// For a binary operator
class BinaryExprAST : public ExprAST {
	char Op;
	ExprAST *LHS, *RHS;
public:
	BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
		: Op(op), LHS(lhs), RHS(rhs) {}
	virtual Value *Codegen();
};

// For a function
class CallExprAST : public ExprAST {
	std::string Callee;
	std::vector<ExprAST*> Args;
public:
	CallExprAST(const std::string &callee, std::vector<ExprAST*> &args)
		: Callee(callee), Args(args) {}
	virtual Value *Codegen();
};

// if/then/else
class IfExprAST : public ExprAST {
	ExprAST *Cond, *Then, *Else;
public:
	IfExprAST(ExprAST *cond, ExprAST *then, ExprAST *_else)
		: Cond(cond), Then(then), Else(_else) {}
	virtual Value *Codegen();
};

// for/in
class ForExprAST : public ExprAST {
	std::string VarName;
	ExprAST *Start, *End, *Step, *Body;
public:
	ForExprAST(const std::string &varname, ExprAST *start, ExprAST *end,
		ExprAST *step, ExprAST *body)
		: VarName(varname), Start(start), End(end), Step(step), Body(body) {}
	virtual Value *Codegen();
};

// Unary operator
class UnaryExprAST : public ExprAST {
	char Opcode;
	ExprAST *Operand;
public:
	UnaryExprAST(char opcode, ExprAST *operand)
		: Opcode(opcode), Operand(operand) {}
	virtual Value *Codegen();
};

// Expression class for var/in
class VarExprAST : public ExprAST {
	std::vector<std::pair<std::string, ExprAST*> > VarNames;
	ExprAST *Body;
public:
	VarExprAST(const std::vector<std::pair<std::string, ExprAST*> > &varnames, ExprAST *body)
		: VarNames(varnames), Body(body) {}
	virtual Value *Codegen();
};

// Prototype for a function (name, arg names, number of args)
class PrototypeAST {
	std::string Name;
	std::vector<std::string> Args;
	bool isOperator;
	unsigned Precedence; // Precedence if a binary op.
public:
	PrototypeAST(const std::string &name, const std::vector<std::string> &args,
		bool isoperator = false, unsigned prec = 0)
		: Name(name), Args(args), isOperator(isoperator), Precedence(prec) {}
	virtual Function *Codegen();

	bool isUnaryOp() const { return isOperator && Args.size() == 1; }
	bool isBinaryOp() const { return isOperator && Args.size() == 2; }

	char getOperatorName() const {
		assert(isUnaryOp() || isBinaryOp());
		return Name[Name.size() - 1];
	}
	unsigned getBinaryPrecedence() const { return Precedence; }
	void CreateArgumentAllocas(Function *F);
};

// The function definition
class FunctionAST {
	PrototypeAST *Proto;
	ExprAST *Body;
public:
	FunctionAST(PrototypeAST *proto, ExprAST *body)
		: Proto(proto), Body(body) {}
	virtual Function *Codegen();
};
