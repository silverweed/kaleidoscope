#include "ast.hpp"

using namespace llvm;

extern ExprAST* Error(const char *Str);
extern FunctionPassManager *TheFPM;

FunctionAST* ErrorF(const char *Str) {
	Error(Str);
	return 0;
}
Value* ErrorV(const char *Str) {
	Error(Str);
	return 0;
}
Value* ErrorV(const char *Str);
extern Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
// symbol table
static std::map<std::string, AllocaInst*> NamedValues;
extern std::map<char, int> BinopPrecedence;

// CreateEntryBlockAlloca - creates an alloca instruction in the entry block
// of the function. Used for mutable vars etc
static AllocaInst* CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) {
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()), 0, VarName.c_str());
}

Value* NumberExprAST::Codegen() {
	// Numeric constant holding an Arbitrary Precision Float
	return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

Value* VariableExprAST::Codegen() {
	Value *V = NamedValues[Name];
	if (!V) return ErrorV(std::string("Unknown variable name: ").append(Name).c_str());
	// Load the value
	return Builder.CreateLoad(V, Name.c_str());
}

Value* BinaryExprAST::Codegen() {
	// Special case '=' because we don't want to emit the LHS as an expression
	if (Op == '=') {
		// Assignment requires the LHS to be an identifier
		VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(LHS);
		if (!LHSE)
			return ErrorV("destination of '=' must be a variable!");
		// Codegen RHS
		Value *Val = RHS->Codegen();
		if (!Val) return 0;

		// Lookup the name
		Value *Variable = NamedValues[LHSE->getName()];
		if (!Variable) return ErrorV(std::string("Unknown variable name: ").append(LHSE->getName()).c_str());

		Builder.CreateStore(Val, Variable);
		return Val;
	}
	Value *L = LHS->Codegen();
	Value *R = RHS->Codegen();
	if (L == 0 || R == 0) return 0;

	switch (Op) {
	case '+': return Builder.CreateFAdd(L, R, "addtmp");
	case '-': return Builder.CreateFSub(L, R, "subtmp");
	case '*': return Builder.CreateFMul(L, R, "multmp");
	case '/': return Builder.CreateFDiv(L, R, "divtmp");
	case '<':
		L = Builder.CreateFCmpULT(L, R, "cmptmp");
		// Convert bool 0/1 to double 0.0 or 1.0
		return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
	default: break;
	}

	// If it wasn't a builtin binary op, it must be an user-defined one.
	// Emit a call to it.
	Function *F = TheModule->getFunction(std::string("binary") + Op);
	assert(F && "binary operator not found!");

	Value *Ops[2] = { L, R };
	return Builder.CreateCall(F, Ops, "binops");
}

Value* CallExprAST::Codegen() {
	// Look up the name in the global module table
	Function *CalleeF = TheModule->getFunction(Callee);
	if (!CalleeF)
		return ErrorV(std::string("Unknown function referenced: ").append(Callee).c_str());
	
	// Argument mismatch error
	if (CalleeF->arg_size() != Args.size())
		return ErrorV("Incorrect number of arguments passed");
	
	std::vector<Value*> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->Codegen());
		if (ArgsV.back() == 0) return 0;
	}
	
	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Value* IfExprAST::Codegen() {
	Value *CondV = Cond->Codegen();
	if (!CondV) return 0;

	// Convert condition to a bool by comparing equal to 0.0
	CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "ifcond");

	// Get the current Function which is being built
	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create blocks for the 'then' and 'else' cases. Insert the 'then' block
	// at the end of the function
	BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
	BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");

	// Create the conditional branch
	Builder.CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit 'then' value
	Builder.SetInsertPoint(ThenBB);
	Value *ThenV = Then->Codegen();
	if(!ThenV) return 0;

	// Unconditional branch to the Merge block. LLVM requires that all blocks are
	// terminated with a control flow instruction, such as a return or a branch.
	Builder.CreateBr(MergeBB);
	// Codegen of 'then' can change the current block; update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();
	
	// Emit 'else' block
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);
	Value *ElseV = Else->Codegen();
	if (!ElseV) return 0;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();

	// Emit 'merge' block
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
	PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, "iftmp");
	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	
	return PN;
}

Value* ForExprAST::Codegen() {

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create an alloca for the variable in the entry block
	AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

	// Emit the start code first, without 'variable' in scope.
	Value *StartVal = Start->Codegen();
	if (!StartVal) return 0;

	// Store the value into the alloca
	Builder.CreateStore(StartVal, Alloca);

	// Make the new basic block for the loop header, inserting after current block.
	BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);

	// Insert an explicit fall through from the current block to the LoopBB
	Builder.CreateBr(LoopBB);

	// Start insertion in LoopBB
	Builder.SetInsertPoint(LoopBB);

	// Within the loop, the variable is defined equal to the PHI node.
	// If it shadows an existing variable, we have to restore it, so save it now.
	AllocaInst *OldVal = NamedValues[VarName];
	NamedValues[VarName] = Alloca;
	
	// Emit the body of the loop. This, like any other expr, can change 
	// the current BB. Note that we ignore the value computed by the body,
	// but don't allow an error
	if (Body->Codegen() == 0) return 0;
	
	// Emit the step value
	Value *StepVal;
	if (Step) {
		StepVal = Step->Codegen();
		if (!StepVal) return 0;
	} else {
		// If not specified, use 1.
		StepVal = ConstantFP::get(getGlobalContext(), APFloat(1.0));
	}

	// Compute the end condition
	Value *EndCond = End->Codegen();
	if (!EndCond) return 0;

	// Reload, increment and restore the alloca. This handles the case where
	// the body of the loop mutates the variable
	Value *CurVar = Builder.CreateLoad(Alloca);
	// Increment the loop variable by StepVal
	Value *NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
	Builder.CreateStore(NextVar, Alloca);

	// Convert condition to a bool by comparing equal to 0.0
	EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "loopcond");
	
	// Create the "after loop" block and insert it
	BasicBlock *LoopEndBB = Builder.GetInsertBlock();
	BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(), "afterloop", TheFunction);

	// Insert the conditional branch into the end of LoopEndBB
	Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

	// Any new code will be inserted in AfterBB
	Builder.SetInsertPoint(AfterBB);

	// Restore the unshadowed variable
	if (OldVal)
		NamedValues[VarName] = OldVal;
	else
		NamedValues.erase(VarName);
	
	// for expr always returns 0.0
	return Constant::getNullValue(Type::getDoubleTy(getGlobalContext()));
}

Value* UnaryExprAST::Codegen() {
	Value *OperandV = Operand->Codegen();
	if (!OperandV) return 0;

	Function *F = TheModule->getFunction(std::string("unary") + Opcode);
	if (!F)
		return ErrorV("Unknown unary operator");
	
	return Builder.CreateCall(F, OperandV, "unop");
}

Value* VarExprAST::Codegen() {
	std::vector<AllocaInst*> OldBindings;
	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Register all variables and emit their initializer
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
		const std::string &VarName = VarNames[i].first;
		ExprAST *Init = VarNames[i].second;
		
		// Emit the initializer before adding the variable to scope; this
		// prevents the initializer from referencing the variable itself,
		// and permits stuff like: 
		//   var a = 1 in
		//      var a = a in # refers to outer 'a'
		Value *InitVal;
		if (Init) {
			InitVal = Init->Codegen();
			if (!InitVal) return 0;
		} else {
			// if initializer in unspecified, default to 0.0
			InitVal = ConstantFP::get(getGlobalContext(), APFloat(0.0));
		}

		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
		Builder.CreateStore(InitVal, Alloca);

		// Remember the old variable binding so that we can restore it
		// when we unrecurse.
		OldBindings.push_back(NamedValues[VarName]);

		// Remember this binding.
		NamedValues[VarName] = Alloca;
	}

	// Codegen the body, now that all vars are in scope
	Value *BodyVal = Body->Codegen();
	if (!BodyVal) return 0;

	// Pop all our variables from scope
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i)
		NamedValues[VarNames[i].first] = OldBindings[i];

	return BodyVal;
}

Function* PrototypeAST::Codegen() {
	// Function type:  double func(double, double, ...)
	std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalContext()));
	FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);

	Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);

	// If F is conflicted, there was already something named 'Name'.
	// If it has a body, don't allow redefinition or reextern.
	if (F->getName() != Name) {
		// Delete the one we just made and get the existing one
		F->eraseFromParent();
		F = TheModule->getFunction(Name);
		// If F already has a body, reject this
		if (!F->empty()) {
			ErrorF("redefinition of function");
			return 0;
		}
		// If F took a different number of args, reject
		if (F->arg_size() != Args.size()) {
			ErrorF("redefinition of function with different number of args");
			return 0;
		}
	}
	// Set names for all arguments
	unsigned Idx = 0;
	for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI, ++Idx) {
		AI->setName(Args[Idx]);
	}
	return F;
}

// Create an alloca for each argument and register the argument in the
// symbol table so that references to it will succeed.
void PrototypeAST::CreateArgumentAllocas(Function *F) {
	Function::arg_iterator AI = F->arg_begin();
	for (unsigned Idx = 0, e = Args.size(); Idx != e; ++Idx, ++AI) {
		// Create an alloca for this variable
		AllocaInst *Alloca = CreateEntryBlockAlloca(F, Args[Idx]);

		// Store the initial value into the alloca
		Builder.CreateStore(AI, Alloca);

		// Add arguments to variable symbol table
		NamedValues[Args[Idx]] = Alloca;
	}
}

Function* FunctionAST::Codegen() {
	NamedValues.clear();
	Function *TheFunction = Proto->Codegen();
	if (!TheFunction)
		return 0;
	
	// If this is an operator, install it
	if (Proto->isBinaryOp())
		BinopPrecedence[Proto->getOperatorName()] = Proto->getBinaryPrecedence();

	// Create a new basic block to start insertion into
	BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	// Add all arguments to the symbol table and create their allocas
	Proto->CreateArgumentAllocas(TheFunction);

	if (Value *RetVal = Body->Codegen()) {
		// Finish off the function
		Builder.CreateRet(RetVal);
		// Validate the code
		verifyFunction(*TheFunction);
		
		// Optimize code
		TheFPM->run(*TheFunction);

		return TheFunction;
	}
	// Error reading body
	// FIXME: if PrototypeAST::Codegen() returns a previously defined forward declaration
	// this code can delete it!
	TheFunction->eraseFromParent();

	if(Proto->isBinaryOp())
		BinopPrecedence.erase(Proto->getOperatorName());

	return 0;
}
