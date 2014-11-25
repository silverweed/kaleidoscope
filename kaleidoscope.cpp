#include <string>
#include <cstdio>
#include <cstdlib>
#include <map>
#include "ast.hpp"

using namespace llvm;

Module *TheModule;
FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;

static int gettok();
static int getNextToken();
static ExprAST* ParseNumberExpr();
static ExprAST* ParseParenExpr();
static ExprAST* ParseIdentifierExpr();
static ExprAST* ParsePrimary();
static ExprAST* ParseUnary();
static int GetTokPrecedence();
static ExprAST* ParseExpression();
static ExprAST* ParseBinOpRHS(int ExprPrec, ExprAST *LHS);
static ExprAST* ParseIfExpr();
static ExprAST* ParseForExpr();
static ExprAST* ParseVarExpr();
static PrototypeAST* ParsePrototype();
static FunctionAST* ParseDefinition();
static PrototypeAST* ParseExtern();
static FunctionAST* ParseTopLevelExpr();
static void MainLoop();
static void HandleDefinition();
static void HandleExtern();
static void HandleTopLevelExpression();

enum Token {
	tok_eof = -1,
	// commands
	tok_def = -2, tok_extern = -3,
	// primary
	tok_identifier = -4, tok_number = -5,
	// flow control
	tok_if = -6, tok_then = -7, tok_else = -8,
	tok_for = -9, tok_in = -10,
	// user-defined operators
	tok_binary = -11, tok_unary = -12,
	// var definition
	tok_var = -13,
};

static std::string IdentifierStr;
static double NumVal;
static int Lineno = 0;

//// Lexer function
static int gettok() {
	static int lastChar = ' ';

	// Skip whitespace
	while (isspace(lastChar)) {
		if (lastChar == '\n') Lineno++;
		lastChar = getchar();
	}
	
	if (isalpha(lastChar)) { // [a-zA-Z][a-zA-Z0-9]*
		IdentifierStr = lastChar;
		while (isalnum((lastChar = getchar())))
			IdentifierStr += lastChar;

		if (IdentifierStr == "def") return tok_def;
		if (IdentifierStr == "extern") return tok_extern;
		if (IdentifierStr == "if") return tok_if;
		if (IdentifierStr == "then") return tok_then;
		if (IdentifierStr == "else") return tok_else;
		if (IdentifierStr == "for") return tok_for;
		if (IdentifierStr == "in") return tok_in;
		if (IdentifierStr == "binary") return tok_binary;
		if (IdentifierStr == "unary") return tok_unary;
		if (IdentifierStr == "var") return tok_var;
		return tok_identifier;
	}
	if (isdigit(lastChar) || lastChar == '.') { // number: [0-9.]+
		std::string numStr;
		do {
			numStr += lastChar;
			lastChar = getchar();
		} while (isdigit(lastChar) || lastChar == '.');

		NumVal = strtod(numStr.c_str(), 0);
		return tok_number;
	}
	if (lastChar == '#') {
		// Comment to EOL
		do lastChar = getchar(); while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');
		if (lastChar != EOF)
			return gettok();
	}
	if (lastChar == EOF)
		return tok_eof;
	
	// Return the character as its ascii value
	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;
}

//// Parser
static int CurTok;
static int getNextToken() {
	return CurTok = gettok();
}

// Error handling functions
ExprAST* Error(const char *Str) {
	fprintf(stderr, "[line %d] Error: %s\n", Lineno, Str);
	return 0;
}
PrototypeAST* ErrorP(const char *Str) {
	Error(Str);
	return 0;
}

// numberexpr ::= number
static ExprAST* ParseNumberExpr() {
	ExprAST *Result = new NumberExprAST(NumVal);
	getNextToken(); // consume the number
	return Result;
}
// parenexpr ::= '(' expression ')'
static ExprAST* ParseParenExpr() {
	getNextToken(); // eat '('
	ExprAST *V = ParseExpression();
	if (!V) return 0;

	if (CurTok != ')')
		return Error("expected ')'");
	getNextToken(); // eat ')'
	return V;
}
// identifierexpr ::= identifier | identifier '(' expression* ')'
static ExprAST* ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;
	getNextToken(); // eat identifier

	// Look ahead to determine whether this identifier is a simple
	// variable reference or a function call.
	if (CurTok != '(') // Simple variable ref
		return new VariableExprAST(IdName);
	
	// Call
	getNextToken(); // eat (
	std::vector<ExprAST*> Args;
	if (CurTok != ')') {
		while (true) {
			ExprAST *Arg = ParseExpression();
			if (!Arg) return 0;
			Args.push_back(Arg);

			if (CurTok == ')') break;

			if (CurTok != ',')
				return Error("Expected ')' or ',' after argument");
			getNextToken();
		}
	}

	// eat the )
	getNextToken();
	return new CallExprAST(IdName, Args);
}
// primary ::= 
//	identifierexpr 
//	| numberexpr 
//	| parenexpr 
//	| ifexpr
//	| forexpr
//	| varexpr
static ExprAST* ParsePrimary() {
	switch (CurTok) {
	case tok_identifier: return ParseIdentifierExpr();
	case tok_number: return ParseNumberExpr();
	case '(': return ParseParenExpr();
	case tok_if: return ParseIfExpr();
	case tok_for: return ParseForExpr();
	case tok_var: return ParseVarExpr();
	default: return Error("encountered unknown token when expecting an expression.");
	}
}

/// Binary expressions
std::map<char, int> BinopPrecedence;

// Get the precedence of the pending binary operator token
static int GetTokPrecedence() {
	if (!isascii(CurTok)) return -1;

	// Ensure it's a declared binop
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0) return -1;
	return TokPrec;
}

// expression ::= primary binoprhs
static ExprAST* ParseExpression() {
	ExprAST *LHS = ParseUnary();
	if (!LHS) return 0;

	return ParseBinOpRHS(0, LHS);
}

// binoprhs ::= (binop primary)*
static ExprAST* ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
	// If this is a binop, find its precedence
	while (true) {
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)
			return LHS;
		
		int BinOp = CurTok;
		getNextToken(); // eat binop

		// Parse the primary expression after the binop
		ExprAST *RHS = ParsePrimary();
		if (!RHS) return 0;
		
		// If BinOp binds less tightly with RHS than the operator after RHS,
		// let the pending operator take RHS as its LHS
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, RHS);
			if (RHS == 0) return 0;
		}
		
		// Merge LHS/RHS
		LHS = new BinaryExprAST(BinOp, LHS, RHS);
	}
}

// ifexpr ::= 'if' expression 'then' expression 'else' expression
static ExprAST* ParseIfExpr() {
	getNextToken(); // eat the 'if'

	// condition
	ExprAST *Cond = ParseExpression();
	if (!Cond) return 0;
	
	// then
	if (CurTok != tok_then)
		return Error("expected 'then' after 'if' condition");
	getNextToken();

	ExprAST *Then = ParseExpression();
	if (!Then) return 0;

	// else
	if (CurTok != tok_else)
		return Error("expected 'else' after 'then' clause");
	getNextToken();

	ExprAST *Else = ParseExpression();
	if (!Else) return 0;

	return new IfExprAST(Cond, Then, Else);
}

// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static ExprAST* ParseForExpr() {
	getNextToken(); // eat the 'for'

	if (CurTok != tok_identifier)
		return Error("Expected identifier after 'for'");
	
	std::string IdName = IdentifierStr;
	getNextToken();

	if (CurTok != '=')
		return Error("Expected '=' after identifier");
	getNextToken();

	ExprAST *Start = ParseExpression();
	if (!Start) return 0;
	if (CurTok != ',')
		return Error("Expected ',' after for start value");
	getNextToken();

	ExprAST  *End = ParseExpression();
	if (!End) return 0;

	// The step value is optional
	ExprAST *Step = 0;
	if (CurTok == ',') {
		getNextToken();
		Step = ParseExpression();
		if (!Step) return 0;
	}

	if (CurTok != tok_in)
		return Error("Expected 'in' after 'for'");
	getNextToken(); // eat 'in'

	ExprAST *Body = ParseExpression();
	if (!Body) return 0;

	return new ForExprAST(IdName, Start, End, Step, Body);
}

// varexpr ::= 'var' identifier ('=' expression)? (',' identifier ...)* 'in' expression
static ExprAST* ParseVarExpr() {
	getNextToken();

	std::vector<std::pair<std::string, ExprAST*> > VarNames;

	// At least one variable name is required
	if (CurTok != tok_identifier)
		return Error("Expected identifier after 'var' keyword");
	
	while (true) {
		std::string Name = IdentifierStr;
		getNextToken(); // eat identifier

		// Read the optional initializer
		ExprAST *Init = 0;
		if (CurTok == '=') {
			getNextToken();
			Init = ParseExpression();
			if (!Init) return 0;
		}

		VarNames.push_back(std::make_pair(Name, Init));
		
		// End of var list, exit loop
		if (CurTok != ',') break;
		
		// Else, parse other variable definitions
		getNextToken();
		if (CurTok != tok_identifier)
			return Error("Expected identifier after 'var' keyword.");
	}

	// At this point, we have to have 'in'
	if (CurTok != tok_in)
		return Error("Expected 'in' after variable declaration");
	getNextToken();

	ExprAST *Body = ParseExpression();
	if (!Body) return 0;

	return new VarExprAST(VarNames, Body);
}

// unary ::= primary | '!' unary
static ExprAST* ParseUnary() {
	// If the current token is not an operator, it must be a primary expr
	if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
		return ParsePrimary();
	
	// If this is an unary operator, read it
	int Opc = CurTok;
	getNextToken();
	if (ExprAST *Operand = ParseUnary())
		return new UnaryExprAST(Opc, Operand);
	return 0;
}

// prototype ::= id '(' id* ')' | binary LETTER number? (id, id)
static PrototypeAST* ParsePrototype() {
	std::string FnName;
	unsigned Kind = 0; // 0: identifier, 1: unary, 2: binary
	unsigned BinaryPrecedence = 30;

	switch (CurTok) {
	case tok_identifier:
		FnName = IdentifierStr;
		Kind = 0;
		getNextToken();
		break;
	case tok_unary:
		getNextToken();
		if (!isascii(CurTok))
			return ErrorP("Expected unary operator");
		FnName = "unary";
		FnName += (char)CurTok;
		Kind = 1;
		getNextToken();
		break;
	case tok_binary:
		getNextToken();
		if (!isascii(CurTok))
			return ErrorP("Expected binary operator");
		FnName = "binary";
		FnName += (char)CurTok;
		Kind = 2;
		getNextToken();
		// Read the precedence if present
		if (CurTok == tok_number) {
			if (NumVal < 1 || NumVal > 100)
				return ErrorP("Invalid precedence: must be 1..100");
			BinaryPrecedence = (unsigned)NumVal;
			getNextToken();
		}
		break;
	default:
		return ErrorP("Expected function name in prototype");
	}
	
	if (CurTok != '(')
		return ErrorP("Expected '(' in prototype");
	
	// Read the list of argument names
	std::vector<std::string> ArgNames;
	while (getNextToken() == tok_identifier)
		ArgNames.push_back(IdentifierStr);

	if (CurTok != ')')
		return ErrorP("Expected ')' in prototype");
	
	getNextToken();
	
	// Verify right number of names for operator
	if (Kind && ArgNames.size() != Kind)
		return ErrorP("Invalid number of operands for operator");

	return new PrototypeAST(FnName, ArgNames, Kind != 0, BinaryPrecedence);
}
// definition ::= 'def' prototype expression
static FunctionAST* ParseDefinition() {
	getNextToken(); // eat 'def'
	PrototypeAST *Proto = ParsePrototype();
	if (!Proto) return 0;

	if (ExprAST *E = ParseExpression())
		return new FunctionAST(Proto, E);
	return 0;
}

// external ::= 'extern' prototype
static PrototypeAST* ParseExtern() {
	getNextToken();
	return ParsePrototype();
}
// toplevelexpr ::= expression
static FunctionAST* ParseTopLevelExpr() {
	if (ExprAST *E = ParseExpression()) {
		// Make an anonymouse prototype
		PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
		return new FunctionAST(Proto, E);
	}
	return 0;
}

/// Driver
// top ::= definition | external | expression | ';'
static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case tok_eof:     return;
		case ';':         getNextToken(); break; // ignore top-level semicolons
		case tok_def:     HandleDefinition(); break;
		case tok_extern:  HandleExtern(); break;
		default:          HandleTopLevelExpression(); break;
		}
	}
}

static void HandleDefinition() {
	if (FunctionAST *F = ParseDefinition()) {
		if (Function *LF = F->Codegen()) {
			fprintf(stderr, "Parsed a definition.\n");
			LF->dump();
		}
	}
	else
		getNextToken();
}
static void HandleExtern() {
	if (PrototypeAST *P = ParseExtern()) {
		if (Function *F = P->Codegen()) {
			fprintf(stderr, "Parsed an extern.\n");
			F->dump();
		}
	}
	else
		getNextToken();
}
static void HandleTopLevelExpression() {
	if (FunctionAST *F = ParseTopLevelExpr()) {
		if (Function *LF = F->Codegen()) {
			fprintf(stderr, "Parsed a top level expr.\n");
			LF->dump();

			// JIT-compile the function, returning a function pointer
			void *FPtr = TheExecutionEngine->getPointerToFunction(LF);

			// Cast it to the right type, so we can call it as a native function
			// (type is double func())
			double (*FP)() = (double (*)())(intptr_t)FPtr;
			fprintf(stderr, "==> %f\n", FP());
		}
	}
	else
		getNextToken();
}

// "Library"
extern "C"
double putchard(double x) {
	putchar((char)x);
	return 0.;
}
extern "C"
double printd(double x) {
	printf("%f\n", x);
	return 0.;
}
//----

int main() {
	LLVMContext &Context = getGlobalContext();

	// Install standard binary operators
	BinopPrecedence['='] = 2;
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;
	BinopPrecedence['/'] = 40;
	// ...

	fprintf(stderr, "ready> ");
	getNextToken();

	InitializeNativeTarget();
	TheModule = new Module("Kaleidoscope", Context);
	// The JIT, which takes ownership of the module
	std::string errStr;
	TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&errStr).create();
	if (!TheExecutionEngine) {
		fprintf(stderr, "Couldn't create engine: %s\n", errStr.c_str());
		return 1;
	}

	// Add optimization
	FunctionPassManager OurFPM(TheModule);
	// Setup optimizer pipeline
	OurFPM.add(new DataLayout(*TheExecutionEngine->getDataLayout()));
	OurFPM.add(createBasicAliasAnalysisPass());
	// Promote allocas to registers
	OurFPM.add(createPromoteMemoryToRegisterPass());
	OurFPM.add(createInstructionCombiningPass());
	OurFPM.add(createReassociatePass());
	OurFPM.add(createGVNPass());
	OurFPM.add(createCFGSimplificationPass());

	OurFPM.doInitialization();
	TheFPM = &OurFPM;

	MainLoop();

	TheModule->dump();
	return 0;
}
