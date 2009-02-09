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

#include "ASTConsumers.h"
#include "clang/Rewrite/Rewriter.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/TranslationUnit.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/Diagnostic.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;

static llvm::cl::opt<bool>
SilenceRewriteMacroWarning("Wno-rewrite-pret", llvm::cl::init(false),
                           llvm::cl::desc("Silence PRET rewriting warnings"));

namespace {
  class RewritePRET : public ASTConsumer {
    Rewriter Rewrite;
    Diagnostic &Diags;
    const LangOptions &LangOpts;
    unsigned RewriteFailedDiag;
    unsigned TryFinallyContainsReturnDiag;
        
    ASTContext *Context;
    SourceManager *SM;
    TranslationUnitDecl *TUDecl;
    FileID MainFileID;
    const char *MainFileStart, *MainFileEnd;
    SourceLocation LastIncLoc;
    
    std::string InFileName;
    std::string OutFileName;
     
    std::string Preamble;
  public:
    virtual void Initialize(ASTContext &context);

    virtual void InitializeTU(TranslationUnit &TU) {
      TU.SetOwnsDecls(false);
      Initialize(TU.getContext());
    }
    

    // Top Level Driver code.
    virtual void HandleTopLevelDecl(Decl *D);
    RewritePRET(std::string inFile, std::string outFile,
                Diagnostic &D, const LangOptions &LOpts);

    ~RewritePRET() {}
    
    virtual void HandleTranslationUnit(TranslationUnit& TU);

    void InsertText(SourceLocation Loc, const char *StrData, unsigned StrLen,
                    bool InsertAfter = true) {
      // If insertion succeeded or warning disabled return with no warning.
      if (!Rewrite.InsertText(Loc, StrData, StrLen, InsertAfter) ||
          SilenceRewriteMacroWarning)
        return;
      
      Diags.Report(Context->getFullLoc(Loc), RewriteFailedDiag);
    }
    
    void RemoveText(SourceLocation Loc, unsigned StrLen) {
      // If removal succeeded or warning disabled return with no warning.
      if (!Rewrite.RemoveText(Loc, StrLen) || SilenceRewriteMacroWarning)
        return;
      
      Diags.Report(Context->getFullLoc(Loc), RewriteFailedDiag);
    }

    void ReplaceText(SourceLocation Start, unsigned OrigLength,
                     const char *NewStr, unsigned NewLength) {
      // If removal succeeded or warning disabled return with no warning.
      if (!Rewrite.ReplaceText(Start, OrigLength, NewStr, NewLength) ||
          SilenceRewriteMacroWarning)
        return;
      
      Diags.Report(Context->getFullLoc(Start), RewriteFailedDiag);
    }

  };
}

RewritePRET::RewritePRET(std::string inFile, std::string outFile,
                         Diagnostic &D, const LangOptions &LOpts)
      : Diags(D), LangOpts(LOpts) {
  InFileName = inFile;
  OutFileName = outFile;
}

ASTConsumer *clang::CreatePRETRewriter(const std::string& InFile,
                                       const std::string& OutFile,
                                       Diagnostic &Diags, 
                                       const LangOptions &LOpts) {
  return new RewritePRET(InFile, OutFile, Diags, LOpts);
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
     
  Rewrite.setSourceMgr(Context->getSourceManager());

  Preamble = "#include <setjmp.h>\n";
  Preamble += "#include \"deadline.h\"\n";
}


//===----------------------------------------------------------------------===//
// Top Level Driver Code
//===----------------------------------------------------------------------===//

void RewritePRET::HandleTopLevelDecl(Decl *D) {
}

//===----------------------------------------------------------------------===//
// Function Body / Expression rewriting
//===----------------------------------------------------------------------===//

void RewritePRET::HandleTranslationUnit(TranslationUnit& TU) {
  printf("In HandleTranslationUnit of RewritePRET\n");

  if (Diags.hasErrorOccurred())
    return;

  // Create the output file.
  
  llvm::OwningPtr<llvm::raw_ostream> OwnedStream;
  llvm::raw_ostream *OutFile;
  if (OutFileName == "-") {
    OutFile = &llvm::outs();
  } else if (!OutFileName.empty()) {
    std::string Err;
    OutFile = new llvm::raw_fd_ostream(OutFileName.c_str(), 
                                       // set binary mode (critical for Windoze)
                                       true, 
                                       Err);
    OwnedStream.reset(OutFile);
  } else if (InFileName == "-") {
    OutFile = &llvm::outs();
  } else {
    llvm::sys::Path Path(InFileName);
    Path.eraseSuffix();
    Path.appendSuffix("rewritten.c");
    std::string Err;
    OutFile = new llvm::raw_fd_ostream(Path.toString().c_str(), 
                                       // set binary mode (critical for Windoze)
                                       true, 
                                       Err);
    OwnedStream.reset(OutFile);
  }
  
  InsertText(SM->getLocForStartOfFile(MainFileID), 
             Preamble.c_str(), Preamble.size(), false);

  // FIXME: Add changes
  
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

