//===--- RewritePRET.cpp - Playground for the code rewriter ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Hacks and fun related to the code rewriter.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/ASTConsumers.h"
#include "clang/Rewrite/Rewriter.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;
using llvm::utostr;

namespace {
  class RewritePRET : public ASTConsumer {
    Rewriter Rewrite;
    Diagnostic &Diags;
    const LangOptions &LangOpts;
    unsigned RewriteFailedDiag;
        
    ASTContext *Context;
    SourceManager *SM;
    TranslationUnitDecl *TUDecl;
    FileID MainFileID;
    const char *MainFileStart, *MainFileEnd;
    SourceLocation LastIncLoc;

    std::string InFileName;
    llvm::raw_ostream* OutFile;
     
    std::string Preamble;

    int CurrentDeadlineRegister;
  public:
    virtual void Initialize(ASTContext &context);
    
    // Top Level Driver code.
    virtual void HandleTopLevelDecl(DeclGroupRef D) {
      for (DeclGroupRef::iterator I = D.begin(), E = D.end(); I != E; ++I)
        HandleTopLevelSingleDecl(*I);
    }
    void HandleTopLevelSingleDecl(Decl *D);
    void HandleDeclInMainFile(Decl *D);
    RewritePRET(std::string inFile, llvm::raw_ostream *OS,
                Diagnostic &D, const LangOptions &LOpts);

    ~RewritePRET() {}
    
    virtual void HandleTranslationUnit(ASTContext &C);

    void InsertText(SourceLocation Loc, const char *StrData, unsigned StrLen,
                    bool InsertAfter = true) {
      // If insertion succeeded return with no warning.
      if (!Rewrite.InsertText(Loc, StrData, StrLen, InsertAfter))
        return;
      
      Diags.Report(Context->getFullLoc(Loc), RewriteFailedDiag);
    }

    void ReplaceText(SourceLocation Start, unsigned OrigLength,
                     const char *NewStr, unsigned NewLength) {
      // If removal succeeded return with no warning.
      if (!Rewrite.ReplaceText(Start, OrigLength, NewStr, NewLength))
        return;

      Diags.Report(Context->getFullLoc(Start), RewriteFailedDiag);
    }

    // Expression Rewriting
    Stmt *RewriteFunctionBody(Stmt *S);
    Stmt *RewriteMainBody(Stmt *S);
    Stmt *RewritePRETTryStmt(PRETTryStmt *S);
    Stmt *RewritePRETTimedLoopStmt(PRETTimedLoopStmt *S);
  };
}

RewritePRET::RewritePRET(std::string inFile, llvm::raw_ostream *OS,
                         Diagnostic &D, const LangOptions &LOpts)
      : Diags(D), LangOpts(LOpts), InFileName(inFile), OutFile(OS) {
}

ASTConsumer *clang::CreatePRETRewriter(const std::string& InFile,
                                       llvm::raw_ostream* OS,
                                       Diagnostic &Diags,
                                       const LangOptions &LOpts) {
  return new RewritePRET(InFile, OS, Diags, LOpts);
}

void RewritePRET::Initialize(ASTContext &context) {
  Context = &context;
  SM = &Context->getSourceManager();
  TUDecl = Context->getTranslationUnitDecl();

  // Get the ID and start/end of the main file.
  MainFileID = SM->getMainFileID();
  const llvm::MemoryBuffer *MainBuf = SM->getBuffer(MainFileID);
  MainFileStart = MainBuf->getBufferStart();
  MainFileEnd = MainBuf->getBufferEnd();
     
  Rewrite.setSourceMgr(Context->getSourceManager(), Context->getLangOptions());

  CurrentDeadlineRegister = 0;
  Preamble = "#include <setjmp.h>\n";
  Preamble += "#include \"deadline.h\"\n";
  Preamble += "\n";
}


//===----------------------------------------------------------------------===//
// Top Level Driver Code
//===----------------------------------------------------------------------===//

void RewritePRET::HandleTopLevelSingleDecl(Decl *D) {

  SourceLocation Loc = D->getLocation();
  Loc = SM->getInstantiationLoc(Loc);

  // If this is for a builtin, ignore it.
  if (Loc.isInvalid()) return;

  // No rewriting takes place for included files.
  if (SM->isFromMainFile(Loc))
    return HandleDeclInMainFile(D);
}

//===----------------------------------------------------------------------===//
// Syntactic (non-AST) Rewriting Code
//===----------------------------------------------------------------------===//
Stmt *RewritePRET::RewritePRETTimedLoopStmt(PRETTimedLoopStmt *S) {
  return S;
}

Stmt *RewritePRET::RewritePRETTryStmt(PRETTryStmt *S) {
  //printf("Rewriting a PRET tryin statment.\n");
  SourceLocation startLoc = S->getLocStart();
  const char *startBuf = SM->getCharacterData(startLoc);
  const bool lowerBounded = S->getLowerBound() != NULL;
  const bool upperBounded = S->getUpperBound() != NULL;
  if (!lowerBounded) printf("No lower bound!\n");
  if (!upperBounded) printf("No upper bound!\n");

  std::string buf;
  S->setDeadlineRegister(CurrentDeadlineRegister);
  const std::string deadreg = llvm::utostr(S->getDeadlineRegister());
  const std::string deadbranchreg = llvm::utostr(S->getDeadlineRegister()+1);
  CurrentDeadlineRegister += 2;
  // Arguments to tryin block go to DEAD and DEADBRANCH statements
  if (upperBounded) {
    buf = "if (_setjmp(PRET_jmpbuf_" + deadbranchreg
           + ") == 0) /* tryin block */ {\n";
  } else {
    buf = "if (1) {\n";
  }
  if (lowerBounded) {
    buf += "DEADLOAD" + deadreg;
  } else {
    buf += "_NULL_STMT";
  }
  const char *lParenLoc = strchr(startBuf, '(');
  ReplaceText(startLoc, lParenLoc - startBuf, buf.c_str(), buf.size());
  const char *semicolonLoc = strchr(lParenLoc, ';');
  startLoc = startLoc.getFileLocWithOffset(semicolonLoc - startBuf);
  buf = ");\n";
  if (upperBounded) {
    buf += "DEADLOADBRANCH" + deadbranchreg + "(";
  } else {
    buf += "_NULL_STMT";
  }
  ReplaceText(startLoc, 1, buf.c_str(), buf.size());

  // Add in if and setjmp code
  startLoc = S->getTryBlock()->getLocStart();
  buf = ";";
  ReplaceText(startLoc, 1, buf.c_str(), buf.size());

  // Add DEADEND at end of try block
  buf = "";
  startLoc = S->getTryBlock()->getLocEnd();
  startBuf = SM->getCharacterData(startLoc);
  assert((*startBuf == '}') && "bogus tryin block");
  if (upperBounded) {
    buf += "DEADLOAD" + deadbranchreg + "(0);\n";
  } else {
    buf += "_NULL_STMT();\n";
  }
  if (lowerBounded) {
    buf += "DEADLOAD" + deadreg + "(0);\n";
  } else {
    buf += "_NULL_STMT();\n";
  }
  InsertText(startLoc, buf.c_str(), buf.size());
  // Replace catch stament with an else statment
  startLoc = startLoc.getFileLocWithOffset(1);
  buf = " else /* catch block */ ";
  const char *lBraceLoc = strchr(startBuf, '{');
  ReplaceText(startLoc, lBraceLoc-startBuf-1, buf.c_str(), buf.size());

  return S;
}

Stmt *RewritePRET::RewriteMainBody(Stmt *S) {
  SourceLocation startLoc = S->getLocStart();
  startLoc = startLoc.getFileLocWithOffset(1);
  std::string buf = "";
  for (int i = 0; i < CurrentDeadlineRegister; i++) {
    buf += "register_jmpbuf(";
    buf += llvm::utostr(i);
    buf += ", &PRET_jmpbuf_";
    buf += llvm::utostr(i);
    buf += ");\n";
  }
  InsertText(startLoc, buf.c_str(), buf.size());

  return S;
}

//===----------------------------------------------------------------------===//
// Function Body / Expression rewriting
//===----------------------------------------------------------------------===//

Stmt *RewritePRET::RewriteFunctionBody(Stmt *S) {
  int nextDeadReg = CurrentDeadlineRegister;
  int prevDeadReg = CurrentDeadlineRegister;
  // Perform a bottom up rewrite of all children.
  for (Stmt::child_iterator CI = S->child_begin(), E = S->child_end();
       CI != E; ++CI) {
    if (*CI) {
      CurrentDeadlineRegister = prevDeadReg;
      Stmt *newStmt = RewriteFunctionBody(*CI);
      nextDeadReg = std::max(nextDeadReg, CurrentDeadlineRegister);
      if (newStmt)
        *CI = newStmt;
    }
  }
  //printf("prev: %d\tnext: %d\n", prevDeadReg, nextDeadReg);
  CurrentDeadlineRegister = nextDeadReg;

  if (PRETTryStmt *StmtTry = dyn_cast<PRETTryStmt>(S))
    return RewritePRETTryStmt(StmtTry);
  else if (PRETTimedLoopStmt *StmtTimedLoop = dyn_cast<PRETTimedLoopStmt>(S))
    return RewritePRETTimedLoopStmt(StmtTimedLoop);

  // Return this Stmt without modifications.
  return S;
}

/// HandleDeclInMainFile - This is called for each top-level decl defined in the
/// main file of the input.
void RewritePRET::HandleDeclInMainFile(Decl *D) {
  if (FunctionDecl *FD = dyn_cast<FunctionDecl>(D)) {
    if (CompoundStmt *Body = FD->getCompoundBody(*Context)) {
      Body = cast_or_null<CompoundStmt>(RewriteFunctionBody(Body));
      assert(Body && "Rewriting functions nullified Body");
      SourceLocation typeSpecLoc = FD->getTypeSpecStartLoc();
      std::string funDecl = SM->getCharacterData(typeSpecLoc);
      std::string::size_type endl_loc = funDecl.find( "\n", 0);
      std::string::size_type main_loc = funDecl.find( "main", 0);
      if (endl_loc != std::string::npos && main_loc != std::string::npos
                && main_loc < endl_loc) {
        printf("Found main()\n");
        Body = cast_or_null<CompoundStmt>(RewriteMainBody(Body));
      }
      FD->setBody(Body);
    }
  }
}

void RewritePRET::HandleTranslationUnit(ASTContext &C) {
  printf("In HandleTranslationUnit of RewritePRET\n");

  if (Diags.hasErrorOccurred())
    return;

  for (int i = 0; i < CurrentDeadlineRegister; i++) {
    std::string buf_name = "PRET_jmpbuf_";
    buf_name += llvm::utostr(i);
    Preamble += "jmp_buf " + buf_name + ";\n";
  }
  InsertText(SM->getLocForStartOfFile(MainFileID),
             Preamble.c_str(), Preamble.size(), false);

  // Get the buffer corresponding to MainFileID.  If we haven't changed it, then
  // we are done.
  if (const RewriteBuffer *RewriteBuf =
      Rewrite.getRewriteBufferFor(MainFileID)) {
    //printf("Changed:\n");
    *OutFile << std::string(RewriteBuf->begin(), RewriteBuf->end());
  } else {
    fprintf(stderr, "No changes\n");
  }

  OutFile->flush();
  printf("Done with rewriting\n");
}

