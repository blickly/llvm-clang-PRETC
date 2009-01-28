//===--- DiagnosticAST.h - Diagnostics for the AST library ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_DIAGNOSTICAST_H
#define LLVM_CLANG_DIAGNOSTICAST_H

#include "clang/Basic/DiagnosticAST.h"

namespace clang {
  namespace diag { 
    enum {
#define DIAG(ENUM,FLAGS,DESC) ENUM,
#include "DiagnosticCommonKinds.def"
#define ASTSTART
#include "DiagnosticASTKinds.def"
      NUM_BUILTIN_AST_DIAGNOSTICS
    };
  }  // end namespace diag
}  // end namespace clang

#endif
