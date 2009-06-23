//===--- StmtPRET.h - Classes for PRET timing constructs -------*- PRET -*-===//
//
// This file defines the PRET timing construct AST node classes.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTPRET_H
#define LLVM_CLANG_AST_STMTPRET_H

#include "clang/AST/Stmt.h"
#define DEADLINE_REGISTERS 8

namespace clang {

/// PRETTryStmt - A PRET try-catch block.
class PRETTryStmt : public Stmt {
  enum { LOWERBOUND, UPPERBOUND, TRY, CATCH, END_EXPR };
  Stmt* SubExprs[END_EXPR];
  SourceLocation TryLoc, CatchLoc;
  int DeadlineRegister;

public:
  PRETTryStmt(SourceLocation tryLoc, Expr *lb, Expr *ub, Stmt *tryS,
              SourceLocation catchLoc, Stmt *catchS)
    : Stmt(PRETTryStmtClass), TryLoc(tryLoc), CatchLoc(catchLoc) {
    SubExprs[LOWERBOUND] = reinterpret_cast<Stmt*>(lb);
    SubExprs[UPPERBOUND] = reinterpret_cast<Stmt*>(ub);
    SubExprs[TRY] = tryS;
    SubExprs[CATCH] = catchS;
    DeadlineRegister = DEADLINE_REGISTERS;
  }

  int getDeadlineRegister() const { return DeadlineRegister; }
  void setDeadlineRegister(int dr) {
    if (dr < DEADLINE_REGISTERS) {
      DeadlineRegister = dr;
    } else {
      fprintf(stderr, "Invalid deadline register request: %d\n", dr);
    }
  }

  virtual SourceRange getSourceRange() const {
    return SourceRange(TryLoc, SubExprs[CATCH]->getLocEnd());
  }

  Expr *getLowerBound() { return reinterpret_cast<Expr*>(SubExprs[LOWERBOUND]); }
  const Expr *getLowerBound() const {
    return reinterpret_cast<Expr*>(SubExprs[LOWERBOUND]);
  }
  Expr *getUpperBound() { return reinterpret_cast<Expr*>(SubExprs[UPPERBOUND]); }
  const Expr *getUpperBound() const {
    return reinterpret_cast<Expr*>(SubExprs[UPPERBOUND]);
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

/// PRETTimedLoopStmt - A PRET timedloop.
class PRETTimedLoopStmt : public Stmt {
  enum { LOWERBOUND, UPPERBOUND, LOOP, CATCH, END_EXPR };
  Stmt* SubExprs[END_EXPR];
  SourceLocation LoopLoc, CatchLoc;
  int DeadlineRegister;

public:
  PRETTimedLoopStmt(SourceLocation loopLoc, Expr *lb, Expr *ub, Stmt *loopS,
              SourceLocation catchLoc, Stmt *catchS)
    : Stmt(PRETTimedLoopStmtClass), LoopLoc(loopLoc){
    SubExprs[LOWERBOUND] = reinterpret_cast<Stmt*>(lb);
    SubExprs[UPPERBOUND] = reinterpret_cast<Stmt*>(ub);
    SubExprs[LOOP] = loopS;
    SubExprs[CATCH] = catchS;
    DeadlineRegister = DEADLINE_REGISTERS;
  }

  int getDeadlineRegister() const { return DeadlineRegister; }
  void setDeadlineRegister(int dr) {
    if (dr < DEADLINE_REGISTERS) {
      DeadlineRegister = dr;
    } else {
      fprintf(stderr, "Invalid deadline register request: %d\n", dr);
    }
  }

  virtual SourceRange getSourceRange() const {
    return SourceRange(LoopLoc, SubExprs[CATCH]->getLocEnd());
  }

  Expr *getLowerBound() { return reinterpret_cast<Expr*>(SubExprs[LOWERBOUND]); }
  const Expr *getLowerBound() const {
    return reinterpret_cast<Expr*>(SubExprs[LOWERBOUND]);
  }
  Expr *getUpperBound() { return reinterpret_cast<Expr*>(SubExprs[UPPERBOUND]); }
  const Expr *getUpperBound() const {
    return reinterpret_cast<Expr*>(SubExprs[UPPERBOUND]);
  }
  CompoundStmt *getLoopBlock() {
    return llvm::cast<CompoundStmt>(SubExprs[LOOP]);
  }
  const CompoundStmt *getLoopBlock() const {
    return llvm::cast<CompoundStmt>(SubExprs[LOOP]);
  }
  CompoundStmt *getCatchBlock() {
    return llvm::cast<CompoundStmt>(SubExprs[CATCH]);
  }
  const CompoundStmt *getCatchBlock() const {
    return llvm::cast<CompoundStmt>(SubExprs[CATCH]);
  }

  SourceLocation getLoopLoc() const { return LoopLoc; }
  SourceLocation getCatchLoc() const { return CatchLoc; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == PRETTimedLoopStmtClass;
  }
  static bool classof(const PRETTimedLoopStmt *) { return true; }

  virtual child_iterator child_begin();
  virtual child_iterator child_end();

};

}  // end namespace clang

#endif
