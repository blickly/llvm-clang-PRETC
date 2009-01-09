//===--- StmtPRET.h - Classes for PRET timing constructs -------*- PRET -*-===//
//
// This file defines the PRET timing construct AST node classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTPRET_H
#define LLVM_CLANG_AST_STMTPRET_H

#include "clang/AST/Stmt.h"

namespace clang {

/// PRETTryStmt - A PRET try-catch block.
class PRETTryStmt : public Stmt {
  enum { CONS, TRY, CATCH, END_EXPR };
  Stmt* SubExprs[END_EXPR];
  SourceLocation TryLoc, CatchLoc;

public:
  PRETTryStmt(SourceLocation tryLoc, Expr *cons, Stmt *tryS,
              SourceLocation catchLoc, Stmt *catchS)
    : Stmt(PRETTryStmtClass), TryLoc(tryLoc), CatchLoc(catchLoc) {
    SubExprs[CONS] = reinterpret_cast<Stmt*>(cons);
    SubExprs[TRY] = tryS;
    SubExprs[CATCH] = catchS;
  }

  virtual SourceRange getSourceRange() const {
    return SourceRange(TryLoc, SubExprs[CATCH]->getLocEnd());
  }

  Expr *getConstraint() { return reinterpret_cast<Expr*>(SubExprs[CONS]); }
  const Expr *getConstraint() const {
    return reinterpret_cast<Expr*>(SubExprs[CONS]);
  }
  CompoundStmt *getTryBlock() {
    return llvm::cast<CompoundStmt>(SubExprs[TRY]);
  }
  const CompoundStmt *getTryBlock() const {
    return llvm::cast<CompoundStmt>(SubExprs[TRY]);
  }
  CompoundStmt *getCatchBlock() {
    return llvm::cast<CompoundStmt>(SubExprs[CATCH]);
  }
  const CompoundStmt *getCatchBlock() const {
    return llvm::cast<CompoundStmt>(SubExprs[CATCH]);
  }

  SourceLocation getTryLoc() const { return TryLoc; }
  SourceLocation getCatchLoc() const { return CatchLoc; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == PRETTryStmtClass;
  }
  static bool classof(const PRETTryStmt *) { return true; }

  virtual child_iterator child_begin();
  virtual child_iterator child_end();

};

}  // end namespace clang

#endif
