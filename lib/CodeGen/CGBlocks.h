//===-- CGBlocks.h - state for LLVM CodeGen for blocks ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is the internal state used for llvm translation for block literals.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_CODEGEN_CGBLOCKS_H
#define CLANG_CODEGEN_CGBLOCKS_H

#include "CodeGenTypes.h"
#include "clang/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ExprObjC.h"

#include <vector>
#include <map>

#include "CGBuilder.h"
#include "CGCall.h"
#include "CGValue.h"

namespace llvm {
  class Module;
  class Constant;
  class Function;
  class GlobalValue;
  class TargetData;
  class FunctionType;
  class Value;
}

namespace clang {

namespace CodeGen {

class BlockBase {
public:
    enum {
        BLOCK_NEEDS_FREE =        (1 << 24),
        BLOCK_HAS_COPY_DISPOSE =  (1 << 25),
        BLOCK_HAS_CXX_OBJ =       (1 << 26),
        BLOCK_IS_GC =             (1 << 27),
        BLOCK_IS_GLOBAL =         (1 << 28),
        BLOCK_HAS_DESCRIPTOR =    (1 << 29)
    };
};

class BlockModule : public BlockBase {
  ASTContext &Context;
  llvm::Module &TheModule;
  CodeGenTypes &Types;
  
  ASTContext &getContext() const { return Context; }
  llvm::Module &getModule() const { return TheModule; }
  CodeGenTypes &getTypes() { return Types; }
public:
  llvm::Constant *getNSConcreteGlobalBlock();
  llvm::Constant *getNSConcreteStackBlock();
  int getGlobalUniqueCount() { return ++Block.GlobalUniqueCount; }
  const llvm::Type *getBlockDescriptorType();

  const llvm::Type *getGenericBlockLiteralType();
  const llvm::Type *getGenericExtendedBlockLiteralType();

  /// NSConcreteGlobalBlock - Cached reference to the class pointer for global
  /// blocks.
  llvm::Constant *NSConcreteGlobalBlock;

  /// NSConcreteStackBlock - Cached reference to the class poinnter for stack
  /// blocks.
  llvm::Constant *NSConcreteStackBlock;
  
  const llvm::Type *BlockDescriptorType;
  const llvm::Type *GenericBlockLiteralType;
  const llvm::Type *GenericExtendedBlockLiteralType;
  struct {
    int GlobalUniqueCount;
  } Block;

  BlockModule(ASTContext &C, llvm::Module &M, CodeGenTypes &T)
    : Context(C), TheModule(M), Types(T), NSConcreteGlobalBlock(0),
      NSConcreteStackBlock(0), BlockDescriptorType(0),
      GenericBlockLiteralType(0)  {
    Block.GlobalUniqueCount = 0;
  }
};

class BlockFunction : public BlockBase {
public:
    enum {
    BLOCK_FIELD_IS_OBJECT   =  3,  /* id, NSObject, __attribute__((NSObject)),
                                      block, ... */
    BLOCK_FIELD_IS_BLOCK    =  7,  /* a block variable */
    BLOCK_FIELD_IS_BYREF    =  8,  /* the on stack structure holding the __block
                                      variable */
    BLOCK_FIELD_IS_WEAK     = 16,  /* declared __weak, only used in byref copy
                                      helpers */
    BLOCK_BYREF_CALLER      = 128  /* called from __block (byref) copy/dispose
                                      support routines */
  };
};

}  // end namespace CodeGen
}  // end namespace clang

#endif
