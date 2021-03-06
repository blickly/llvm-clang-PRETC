//===- examples/ModuleMaker/ModuleMaker.cpp - Example project ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This programs is a simple example that creates an LLVM module "from scratch",
// emitting it as a bitcode file to standard out.  This is just to show how
// LLVM projects work and to demonstrate some of the LLVM APIs.
//
//===----------------------------------------------------------------------===//

#include "llvm/Module.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Constants.h"
#include "llvm/Instructions.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include <iostream>
using namespace llvm;

int main() {
  // Create the "module" or "program" or "translation unit" to hold the
  // function
  Module *M = new Module("test");

  // Create the main function: first create the type 'int ()'
  FunctionType *FT = FunctionType::get(Type::Int32Ty, std::vector<const Type*>(),
                                       /*not vararg*/false);

  // By passing a module as the last parameter to the Function constructor,
  // it automatically gets appended to the Module.
  Function *F = Function::Create(FT, Function::ExternalLinkage, "main", M);

  // Add a basic block to the function... again, it automatically inserts
  // because of the last argument.
  BasicBlock *BB = BasicBlock::Create("EntryBlock", F);

  // Get pointers to the constant integers...
  Value *Two = ConstantInt::get(Type::Int32Ty, 2);
  Value *Three = ConstantInt::get(Type::Int32Ty, 3);

  // Create the add instruction... does not insert...
  Instruction *Add = BinaryOperator::Create(Instruction::Add, Two, Three,
                                            "addresult");

  // explicitly insert it into the basic block...
  BB->getInstList().push_back(Add);

  // Create the return instruction and add it to the basic block
  BB->getInstList().push_back(ReturnInst::Create(Add));

  // Output the bitcode file to stdout
  WriteBitcodeToFile(M, std::cout);

  // Delete the module and all of its contents.
  delete M;
  return 0;
}
