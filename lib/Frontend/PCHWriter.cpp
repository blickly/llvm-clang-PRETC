//===--- PCHWriter.h - Precompiled Headers Writer ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the PCHWriter class, which writes a precompiled header.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/PCHWriter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclContextInternals.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/Type.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Bitcode/BitstreamWriter.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MemoryBuffer.h"

using namespace clang;

//===----------------------------------------------------------------------===//
// Type serialization
//===----------------------------------------------------------------------===//
namespace {
  class VISIBILITY_HIDDEN PCHTypeWriter {
    PCHWriter &Writer;
    PCHWriter::RecordData &Record;

  public:
    /// \brief Type code that corresponds to the record generated.
    pch::TypeCode Code;

    PCHTypeWriter(PCHWriter &Writer, PCHWriter::RecordData &Record) 
      : Writer(Writer), Record(Record) { }

    void VisitArrayType(const ArrayType *T);
    void VisitFunctionType(const FunctionType *T);
    void VisitTagType(const TagType *T);

#define TYPE(Class, Base) void Visit##Class##Type(const Class##Type *T);
#define ABSTRACT_TYPE(Class, Base)
#define DEPENDENT_TYPE(Class, Base)
#include "clang/AST/TypeNodes.def"
  };
}

void PCHTypeWriter::VisitExtQualType(const ExtQualType *T) {
  Writer.AddTypeRef(QualType(T->getBaseType(), 0), Record);
  Record.push_back(T->getObjCGCAttr()); // FIXME: use stable values
  Record.push_back(T->getAddressSpace());
  Code = pch::TYPE_EXT_QUAL;
}

void PCHTypeWriter::VisitBuiltinType(const BuiltinType *T) {
  assert(false && "Built-in types are never serialized");
}

void PCHTypeWriter::VisitFixedWidthIntType(const FixedWidthIntType *T) {
  Record.push_back(T->getWidth());
  Record.push_back(T->isSigned());
  Code = pch::TYPE_FIXED_WIDTH_INT;
}

void PCHTypeWriter::VisitComplexType(const ComplexType *T) {
  Writer.AddTypeRef(T->getElementType(), Record);
  Code = pch::TYPE_COMPLEX;
}

void PCHTypeWriter::VisitPointerType(const PointerType *T) {
  Writer.AddTypeRef(T->getPointeeType(), Record);
  Code = pch::TYPE_POINTER;
}

void PCHTypeWriter::VisitBlockPointerType(const BlockPointerType *T) {
  Writer.AddTypeRef(T->getPointeeType(), Record);  
  Code = pch::TYPE_BLOCK_POINTER;
}

void PCHTypeWriter::VisitLValueReferenceType(const LValueReferenceType *T) {
  Writer.AddTypeRef(T->getPointeeType(), Record);
  Code = pch::TYPE_LVALUE_REFERENCE;
}

void PCHTypeWriter::VisitRValueReferenceType(const RValueReferenceType *T) {
  Writer.AddTypeRef(T->getPointeeType(), Record);
  Code = pch::TYPE_RVALUE_REFERENCE;
}

void PCHTypeWriter::VisitMemberPointerType(const MemberPointerType *T) {
  Writer.AddTypeRef(T->getPointeeType(), Record);  
  Writer.AddTypeRef(QualType(T->getClass(), 0), Record);  
  Code = pch::TYPE_MEMBER_POINTER;
}

void PCHTypeWriter::VisitArrayType(const ArrayType *T) {
  Writer.AddTypeRef(T->getElementType(), Record);
  Record.push_back(T->getSizeModifier()); // FIXME: stable values
  Record.push_back(T->getIndexTypeQualifier()); // FIXME: stable values
}

void PCHTypeWriter::VisitConstantArrayType(const ConstantArrayType *T) {
  VisitArrayType(T);
  Writer.AddAPInt(T->getSize(), Record);
  Code = pch::TYPE_CONSTANT_ARRAY;
}

void PCHTypeWriter::VisitIncompleteArrayType(const IncompleteArrayType *T) {
  VisitArrayType(T);
  Code = pch::TYPE_INCOMPLETE_ARRAY;
}

void PCHTypeWriter::VisitVariableArrayType(const VariableArrayType *T) {
  VisitArrayType(T);
  // FIXME: Serialize array size expression.
  assert(false && "Cannot serialize variable-length arrays");
  Code = pch::TYPE_VARIABLE_ARRAY;
}

void PCHTypeWriter::VisitVectorType(const VectorType *T) {
  Writer.AddTypeRef(T->getElementType(), Record);
  Record.push_back(T->getNumElements());
  Code = pch::TYPE_VECTOR;
}

void PCHTypeWriter::VisitExtVectorType(const ExtVectorType *T) {
  VisitVectorType(T);
  Code = pch::TYPE_EXT_VECTOR;
}

void PCHTypeWriter::VisitFunctionType(const FunctionType *T) {
  Writer.AddTypeRef(T->getResultType(), Record);
}

void PCHTypeWriter::VisitFunctionNoProtoType(const FunctionNoProtoType *T) {
  VisitFunctionType(T);
  Code = pch::TYPE_FUNCTION_NO_PROTO;
}

void PCHTypeWriter::VisitFunctionProtoType(const FunctionProtoType *T) {
  VisitFunctionType(T);
  Record.push_back(T->getNumArgs());
  for (unsigned I = 0, N = T->getNumArgs(); I != N; ++I)
    Writer.AddTypeRef(T->getArgType(I), Record);
  Record.push_back(T->isVariadic());
  Record.push_back(T->getTypeQuals());
  Code = pch::TYPE_FUNCTION_PROTO;
}

void PCHTypeWriter::VisitTypedefType(const TypedefType *T) {
  Writer.AddDeclRef(T->getDecl(), Record);
  Code = pch::TYPE_TYPEDEF;
}

void PCHTypeWriter::VisitTypeOfExprType(const TypeOfExprType *T) {
  // FIXME: serialize the typeof expression
  assert(false && "Cannot serialize typeof(expr)");
  Code = pch::TYPE_TYPEOF_EXPR;
}

void PCHTypeWriter::VisitTypeOfType(const TypeOfType *T) {
  Writer.AddTypeRef(T->getUnderlyingType(), Record);
  Code = pch::TYPE_TYPEOF;
}

void PCHTypeWriter::VisitTagType(const TagType *T) {
  Writer.AddDeclRef(T->getDecl(), Record);
  assert(!T->isBeingDefined() && 
         "Cannot serialize in the middle of a type definition");
}

void PCHTypeWriter::VisitRecordType(const RecordType *T) {
  VisitTagType(T);
  Code = pch::TYPE_RECORD;
}

void PCHTypeWriter::VisitEnumType(const EnumType *T) {
  VisitTagType(T);
  Code = pch::TYPE_ENUM;
}

void 
PCHTypeWriter::VisitTemplateSpecializationType(
                                       const TemplateSpecializationType *T) {
  // FIXME: Serialize this type
  assert(false && "Cannot serialize template specialization types");
}

void PCHTypeWriter::VisitQualifiedNameType(const QualifiedNameType *T) {
  // FIXME: Serialize this type
  assert(false && "Cannot serialize qualified name types");
}

void PCHTypeWriter::VisitObjCInterfaceType(const ObjCInterfaceType *T) {
  Writer.AddDeclRef(T->getDecl(), Record);
  Code = pch::TYPE_OBJC_INTERFACE;
}

void 
PCHTypeWriter::VisitObjCQualifiedInterfaceType(
                                      const ObjCQualifiedInterfaceType *T) {
  VisitObjCInterfaceType(T);
  Record.push_back(T->getNumProtocols());
  for (unsigned I = 0, N = T->getNumProtocols(); I != N; ++I)
    Writer.AddDeclRef(T->getProtocol(I), Record);
  Code = pch::TYPE_OBJC_QUALIFIED_INTERFACE;
}

void PCHTypeWriter::VisitObjCQualifiedIdType(const ObjCQualifiedIdType *T) {
  Record.push_back(T->getNumProtocols());
  for (unsigned I = 0, N = T->getNumProtocols(); I != N; ++I)
    Writer.AddDeclRef(T->getProtocols(I), Record);
  Code = pch::TYPE_OBJC_QUALIFIED_ID;
}

void 
PCHTypeWriter::VisitObjCQualifiedClassType(const ObjCQualifiedClassType *T) {
  Record.push_back(T->getNumProtocols());
  for (unsigned I = 0, N = T->getNumProtocols(); I != N; ++I)
    Writer.AddDeclRef(T->getProtocols(I), Record);
  Code = pch::TYPE_OBJC_QUALIFIED_CLASS;
}

//===----------------------------------------------------------------------===//
// Declaration serialization
//===----------------------------------------------------------------------===//
namespace {
  class VISIBILITY_HIDDEN PCHDeclWriter
    : public DeclVisitor<PCHDeclWriter, void> {

    PCHWriter &Writer;
    PCHWriter::RecordData &Record;

  public:
    pch::DeclCode Code;

    PCHDeclWriter(PCHWriter &Writer, PCHWriter::RecordData &Record) 
      : Writer(Writer), Record(Record) { }

    void VisitDecl(Decl *D);
    void VisitTranslationUnitDecl(TranslationUnitDecl *D);
    void VisitNamedDecl(NamedDecl *D);
    void VisitTypeDecl(TypeDecl *D);
    void VisitTypedefDecl(TypedefDecl *D);
    void VisitValueDecl(ValueDecl *D);
    void VisitVarDecl(VarDecl *D);

    void VisitDeclContext(DeclContext *DC, uint64_t LexicalOffset, 
                          uint64_t VisibleOffset);
  };
}

void PCHDeclWriter::VisitDecl(Decl *D) {
  Writer.AddDeclRef(cast_or_null<Decl>(D->getDeclContext()), Record);
  Writer.AddDeclRef(cast_or_null<Decl>(D->getLexicalDeclContext()), Record);
  Writer.AddSourceLocation(D->getLocation(), Record);
  Record.push_back(D->isInvalidDecl());
  // FIXME: hasAttrs
  Record.push_back(D->isImplicit());
  Record.push_back(D->getAccess());
}

void PCHDeclWriter::VisitTranslationUnitDecl(TranslationUnitDecl *D) {
  VisitDecl(D);
  Code = pch::DECL_TRANSLATION_UNIT;
}

void PCHDeclWriter::VisitNamedDecl(NamedDecl *D) {
  VisitDecl(D);
  Writer.AddDeclarationName(D->getDeclName(), Record);
}

void PCHDeclWriter::VisitTypeDecl(TypeDecl *D) {
  VisitNamedDecl(D);
  Writer.AddTypeRef(QualType(D->getTypeForDecl(), 0), Record);
}

void PCHDeclWriter::VisitTypedefDecl(TypedefDecl *D) {
  VisitTypeDecl(D);
  Writer.AddTypeRef(D->getUnderlyingType(), Record);
  Code = pch::DECL_TYPEDEF;
}

void PCHDeclWriter::VisitValueDecl(ValueDecl *D) {
  VisitNamedDecl(D);
  Writer.AddTypeRef(D->getType(), Record);
}

void PCHDeclWriter::VisitVarDecl(VarDecl *D) {
  VisitValueDecl(D);
  Record.push_back(D->getStorageClass());
  Record.push_back(D->isThreadSpecified());
  Record.push_back(D->hasCXXDirectInitializer());
  Record.push_back(D->isDeclaredInCondition());
  Writer.AddDeclRef(D->getPreviousDeclaration(), Record);
  Writer.AddSourceLocation(D->getTypeSpecStartLoc(), Record);
  // FIXME: emit initializer
  Code = pch::DECL_VAR;
}

/// \brief Emit the DeclContext part of a declaration context decl.
///
/// \param LexicalOffset the offset at which the DECL_CONTEXT_LEXICAL
/// block for this declaration context is stored. May be 0 to indicate
/// that there are no declarations stored within this context.
///
/// \param VisibleOffset the offset at which the DECL_CONTEXT_VISIBLE
/// block for this declaration context is stored. May be 0 to indicate
/// that there are no declarations visible from this context. Note
/// that this value will not be emitted for non-primary declaration
/// contexts.
void PCHDeclWriter::VisitDeclContext(DeclContext *DC, uint64_t LexicalOffset, 
                                     uint64_t VisibleOffset) {
  Record.push_back(LexicalOffset);
  if (DC->getPrimaryContext() == DC)
    Record.push_back(VisibleOffset);
}

//===----------------------------------------------------------------------===//
// PCHWriter Implementation
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Source Manager Serialization
//===----------------------------------------------------------------------===//

/// \brief Create an abbreviation for the SLocEntry that refers to a
/// file.
static unsigned CreateSLocFileAbbrev(llvm::BitstreamWriter &S) {
  using namespace llvm;
  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(pch::SM_SLOC_FILE_ENTRY));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Include location
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 2)); // Characteristic
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 1)); // Line directives
  // FIXME: Need an actual encoding for the line directives; maybe
  // this should be an array?
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // File name
  return S.EmitAbbrev(Abbrev);
}

/// \brief Create an abbreviation for the SLocEntry that refers to a
/// buffer.
static unsigned CreateSLocBufferAbbrev(llvm::BitstreamWriter &S) {
  using namespace llvm;
  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(pch::SM_SLOC_BUFFER_ENTRY));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Include location
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 2)); // Characteristic
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Fixed, 1)); // Line directives
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // Buffer name blob
  return S.EmitAbbrev(Abbrev);
}

/// \brief Create an abbreviation for the SLocEntry that refers to a
/// buffer's blob.
static unsigned CreateSLocBufferBlobAbbrev(llvm::BitstreamWriter &S) {
  using namespace llvm;
  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(pch::SM_SLOC_BUFFER_BLOB));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::Blob)); // Blob
  return S.EmitAbbrev(Abbrev);
}

/// \brief Create an abbreviation for the SLocEntry that refers to an
/// buffer.
static unsigned CreateSLocInstantiationAbbrev(llvm::BitstreamWriter &S) {
  using namespace llvm;
  BitCodeAbbrev *Abbrev = new BitCodeAbbrev();
  Abbrev->Add(BitCodeAbbrevOp(pch::SM_SLOC_INSTANTIATION_ENTRY));
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Offset
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Spelling location
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // Start location
  Abbrev->Add(BitCodeAbbrevOp(BitCodeAbbrevOp::VBR, 8)); // End location
  return S.EmitAbbrev(Abbrev);
}

/// \brief Writes the block containing the serialized form of the
/// source manager.
///
/// TODO: We should probably use an on-disk hash table (stored in a
/// blob), indexed based on the file name, so that we only create
/// entries for files that we actually need. In the common case (no
/// errors), we probably won't have to create file entries for any of
/// the files in the AST.
void PCHWriter::WriteSourceManagerBlock(SourceManager &SourceMgr) {
  // Enter the types block
  S.EnterSubblock(pch::SOURCE_MANAGER_BLOCK_ID, 3);

  // Abbreviations for the various kinds of source-location entries.
  int SLocFileAbbrv = -1;
  int SLocBufferAbbrv = -1;
  int SLocBufferBlobAbbrv = -1;
  int SLocInstantiationAbbrv = -1;

  // Write out the source location entry table. We skip the first
  // entry, which is always the same dummy entry.
  RecordData Record;
  for (SourceManager::sloc_entry_iterator 
         SLoc = SourceMgr.sloc_entry_begin() + 1,
         SLocEnd = SourceMgr.sloc_entry_end();
       SLoc != SLocEnd; ++SLoc) {
    // Figure out which record code to use.
    unsigned Code;
    if (SLoc->isFile()) {
      if (SLoc->getFile().getContentCache()->Entry)
        Code = pch::SM_SLOC_FILE_ENTRY;
      else
        Code = pch::SM_SLOC_BUFFER_ENTRY;
    } else
      Code = pch::SM_SLOC_INSTANTIATION_ENTRY;
    Record.push_back(Code);

    Record.push_back(SLoc->getOffset());
    if (SLoc->isFile()) {
      const SrcMgr::FileInfo &File = SLoc->getFile();
      Record.push_back(File.getIncludeLoc().getRawEncoding());
      Record.push_back(File.getFileCharacteristic()); // FIXME: stable encoding
      Record.push_back(File.hasLineDirectives()); // FIXME: encode the
                                                  // line directives?

      const SrcMgr::ContentCache *Content = File.getContentCache();
      if (Content->Entry) {
        // The source location entry is a file. The blob associated
        // with this entry is the file name.
        if (SLocFileAbbrv == -1)
          SLocFileAbbrv = CreateSLocFileAbbrev(S);
        S.EmitRecordWithBlob(SLocFileAbbrv, Record,
                             Content->Entry->getName(),
                             strlen(Content->Entry->getName()));
      } else {
        // The source location entry is a buffer. The blob associated
        // with this entry contains the contents of the buffer.
        if (SLocBufferAbbrv == -1) {
          SLocBufferAbbrv = CreateSLocBufferAbbrev(S);
          SLocBufferBlobAbbrv = CreateSLocBufferBlobAbbrev(S);
        }

        // We add one to the size so that we capture the trailing NULL
        // that is required by llvm::MemoryBuffer::getMemBuffer (on
        // the reader side).
        const llvm::MemoryBuffer *Buffer = Content->getBuffer();
        const char *Name = Buffer->getBufferIdentifier();
        S.EmitRecordWithBlob(SLocBufferAbbrv, Record, Name, strlen(Name) + 1);
        Record.clear();
        Record.push_back(pch::SM_SLOC_BUFFER_BLOB);
        S.EmitRecordWithBlob(SLocBufferBlobAbbrv, Record,
                             Buffer->getBufferStart(),
                             Buffer->getBufferSize() + 1);
      }
    } else {
      // The source location entry is an instantiation.
      const SrcMgr::InstantiationInfo &Inst = SLoc->getInstantiation();
      Record.push_back(Inst.getSpellingLoc().getRawEncoding());
      Record.push_back(Inst.getInstantiationLocStart().getRawEncoding());
      Record.push_back(Inst.getInstantiationLocEnd().getRawEncoding());

      if (SLocInstantiationAbbrv == -1)
        SLocInstantiationAbbrv = CreateSLocInstantiationAbbrev(S);
      S.EmitRecordWithAbbrev(SLocInstantiationAbbrv, Record);
    }

    Record.clear();
  }

  S.ExitBlock();
}

/// \brief Writes the block containing the serialized form of the
/// preprocessor.
///
void PCHWriter::WritePreprocessor(Preprocessor &PP) {
}


/// \brief Write the representation of a type to the PCH stream.
void PCHWriter::WriteType(const Type *T) {
  pch::ID &ID = TypeIDs[T];
  if (ID == 0) // we haven't seen this type before
    ID = NextTypeID++;
  
  // Record the offset for this type.
  if (TypeOffsets.size() == ID - pch::NUM_PREDEF_TYPE_IDS)
    TypeOffsets.push_back(S.GetCurrentBitNo());
  else if (TypeOffsets.size() < ID - pch::NUM_PREDEF_TYPE_IDS) {
    TypeOffsets.resize(ID + 1 - pch::NUM_PREDEF_TYPE_IDS);
    TypeOffsets[ID - pch::NUM_PREDEF_TYPE_IDS] = S.GetCurrentBitNo();
  }

  RecordData Record;
  
  // Emit the type's representation.
  PCHTypeWriter W(*this, Record);
  switch (T->getTypeClass()) {
    // For all of the concrete, non-dependent types, call the
    // appropriate visitor function.
#define TYPE(Class, Base) \
    case Type::Class: W.Visit##Class##Type(cast<Class##Type>(T)); break;
#define ABSTRACT_TYPE(Class, Base)
#define DEPENDENT_TYPE(Class, Base)
#include "clang/AST/TypeNodes.def"

    // For all of the dependent type nodes (which only occur in C++
    // templates), produce an error.
#define TYPE(Class, Base)
#define DEPENDENT_TYPE(Class, Base) case Type::Class:
#include "clang/AST/TypeNodes.def"
    assert(false && "Cannot serialize dependent type nodes");
    break;
  }

  // Emit the serialized record.
  S.EmitRecord(W.Code, Record);
}

/// \brief Write a block containing all of the types.
void PCHWriter::WriteTypesBlock(ASTContext &Context) {
  // Enter the types block
  S.EnterSubblock(pch::TYPES_BLOCK_ID, 2);

  // Emit all of the types in the ASTContext
  for (std::vector<Type*>::const_iterator T = Context.getTypes().begin(),
                                       TEnd = Context.getTypes().end();
       T != TEnd; ++T) {
    // Builtin types are never serialized.
    if (isa<BuiltinType>(*T))
      continue;

    WriteType(*T);
  }

  // Exit the types block
  S.ExitBlock();

  // Write the type offsets block
  S.EnterSubblock(pch::TYPE_OFFSETS_BLOCK_ID, 2);
  S.EmitRecord(pch::TYPE_OFFSET, TypeOffsets);
  S.ExitBlock();
}

/// \brief Write the block containing all of the declaration IDs
/// lexically declared within the given DeclContext.
///
/// \returns the offset of the DECL_CONTEXT_LEXICAL block within the
/// bistream, or 0 if no block was written.
uint64_t PCHWriter::WriteDeclContextLexicalBlock(ASTContext &Context, 
                                                 DeclContext *DC) {
  if (DC->decls_begin(Context) == DC->decls_end(Context))
    return 0;

  uint64_t Offset = S.GetCurrentBitNo();
  RecordData Record;
  for (DeclContext::decl_iterator D = DC->decls_begin(Context),
                               DEnd = DC->decls_end(Context);
       D != DEnd; ++D)
    AddDeclRef(*D, Record);

  S.EmitRecord(pch::DECL_CONTEXT_LEXICAL, Record);
  return Offset;
}

/// \brief Write the block containing all of the declaration IDs
/// visible from the given DeclContext.
///
/// \returns the offset of the DECL_CONTEXT_VISIBLE block within the
/// bistream, or 0 if no block was written.
uint64_t PCHWriter::WriteDeclContextVisibleBlock(ASTContext &Context,
                                                 DeclContext *DC) {
  if (DC->getPrimaryContext() != DC)
    return 0;

  // Force the DeclContext to build a its name-lookup table.
  DC->lookup(Context, DeclarationName());

  // Serialize the contents of the mapping used for lookup. Note that,
  // although we have two very different code paths, the serialized
  // representation is the same for both cases: a declaration name,
  // followed by a size, followed by references to the visible
  // declarations that have that name.
  uint64_t Offset = S.GetCurrentBitNo();
  RecordData Record;
  StoredDeclsMap *Map = static_cast<StoredDeclsMap*>(DC->getLookupPtr());
  for (StoredDeclsMap::iterator D = Map->begin(), DEnd = Map->end();
       D != DEnd; ++D) {
    AddDeclarationName(D->first, Record);
    DeclContext::lookup_result Result = D->second.getLookupResult(Context);
    Record.push_back(Result.second - Result.first);
    for(; Result.first != Result.second; ++Result.first)
      AddDeclRef(*Result.first, Record);
  }

  if (Record.size() == 0)
    return 0;

  S.EmitRecord(pch::DECL_CONTEXT_VISIBLE, Record);
  return Offset;
}

/// \brief Write a block containing all of the declarations.
void PCHWriter::WriteDeclsBlock(ASTContext &Context) {
  // Enter the declarations block
  S.EnterSubblock(pch::DECLS_BLOCK_ID, 2);

  // Emit all of the declarations.
  RecordData Record;
  PCHDeclWriter W(*this, Record);
  while (!DeclsToEmit.empty()) {
    // Pull the next declaration off the queue
    Decl *D = DeclsToEmit.front();
    DeclsToEmit.pop();

    // If this declaration is also a DeclContext, write blocks for the
    // declarations that lexically stored inside its context and those
    // declarations that are visible from its context. These blocks
    // are written before the declaration itself so that we can put
    // their offsets into the record for the declaration.
    uint64_t LexicalOffset = 0;
    uint64_t VisibleOffset = 0;
    DeclContext *DC = dyn_cast<DeclContext>(D);
    if (DC) {
      LexicalOffset = WriteDeclContextLexicalBlock(Context, DC);
      VisibleOffset = WriteDeclContextVisibleBlock(Context, DC);
    }

    // Determine the ID for this declaration
    pch::ID ID = DeclIDs[D];
    if (ID == 0)
      ID = DeclIDs.size();

    unsigned Index = ID - 1;

    // Record the offset for this declaration
    if (DeclOffsets.size() == Index)
      DeclOffsets.push_back(S.GetCurrentBitNo());
    else if (DeclOffsets.size() < Index) {
      DeclOffsets.resize(Index+1);
      DeclOffsets[Index] = S.GetCurrentBitNo();
    }

    // Build and emit a record for this declaration
    Record.clear();
    W.Code = (pch::DeclCode)0;
    W.Visit(D);
    if (DC) W.VisitDeclContext(DC, LexicalOffset, VisibleOffset);
    assert(W.Code && "Visitor did not set record code");
    S.EmitRecord(W.Code, Record);
  }

  // Exit the declarations block
  S.ExitBlock();

  // Write the declaration offsets block
  S.EnterSubblock(pch::DECL_OFFSETS_BLOCK_ID, 2);
  S.EmitRecord(pch::DECL_OFFSET, DeclOffsets);
  S.ExitBlock();
}

PCHWriter::PCHWriter(llvm::BitstreamWriter &S) 
  : S(S), NextTypeID(pch::NUM_PREDEF_TYPE_IDS) { }

void PCHWriter::WritePCH(ASTContext &Context, Preprocessor &PP) {
  // Emit the file header.
  S.Emit((unsigned)'C', 8);
  S.Emit((unsigned)'P', 8);
  S.Emit((unsigned)'C', 8);
  S.Emit((unsigned)'H', 8);

  // The translation unit is the first declaration we'll emit.
  DeclIDs[Context.getTranslationUnitDecl()] = 1;
  DeclsToEmit.push(Context.getTranslationUnitDecl());

  // Write the remaining PCH contents.
  S.EnterSubblock(pch::PCH_BLOCK_ID, 2);
  WriteSourceManagerBlock(Context.getSourceManager());
  WritePreprocessor(PP);
  WriteTypesBlock(Context);
  WriteDeclsBlock(Context);
  S.ExitBlock();
}

void PCHWriter::AddSourceLocation(SourceLocation Loc, RecordData &Record) {
  Record.push_back(Loc.getRawEncoding());
}

void PCHWriter::AddAPInt(const llvm::APInt &Value, RecordData &Record) {
  Record.push_back(Value.getBitWidth());
  unsigned N = Value.getNumWords();
  const uint64_t* Words = Value.getRawData();
  for (unsigned I = 0; I != N; ++I)
    Record.push_back(Words[I]);
}

void PCHWriter::AddIdentifierRef(const IdentifierInfo *II, RecordData &Record) {
  // FIXME: Emit an identifier ID, not the actual string!
  const char *Name = II->getName();
  unsigned Len = strlen(Name);
  Record.push_back(Len);
  Record.insert(Record.end(), Name, Name + Len);
}

void PCHWriter::AddTypeRef(QualType T, RecordData &Record) {
  if (T.isNull()) {
    Record.push_back(pch::PREDEF_TYPE_NULL_ID);
    return;
  }

  if (const BuiltinType *BT = dyn_cast<BuiltinType>(T.getTypePtr())) {
    pch::ID ID;
    switch (BT->getKind()) {
    case BuiltinType::Void:       ID = pch::PREDEF_TYPE_VOID_ID;       break;
    case BuiltinType::Bool:       ID = pch::PREDEF_TYPE_BOOL_ID;       break;
    case BuiltinType::Char_U:     ID = pch::PREDEF_TYPE_CHAR_U_ID;     break;
    case BuiltinType::UChar:      ID = pch::PREDEF_TYPE_UCHAR_ID;      break;
    case BuiltinType::UShort:     ID = pch::PREDEF_TYPE_USHORT_ID;     break;
    case BuiltinType::UInt:       ID = pch::PREDEF_TYPE_UINT_ID;       break;
    case BuiltinType::ULong:      ID = pch::PREDEF_TYPE_ULONG_ID;      break;
    case BuiltinType::ULongLong:  ID = pch::PREDEF_TYPE_ULONGLONG_ID;  break;
    case BuiltinType::Char_S:     ID = pch::PREDEF_TYPE_CHAR_S_ID;     break;
    case BuiltinType::SChar:      ID = pch::PREDEF_TYPE_SCHAR_ID;      break;
    case BuiltinType::WChar:      ID = pch::PREDEF_TYPE_WCHAR_ID;      break;
    case BuiltinType::Short:      ID = pch::PREDEF_TYPE_SHORT_ID;      break;
    case BuiltinType::Int:        ID = pch::PREDEF_TYPE_INT_ID;        break;
    case BuiltinType::Long:       ID = pch::PREDEF_TYPE_LONG_ID;       break;
    case BuiltinType::LongLong:   ID = pch::PREDEF_TYPE_LONGLONG_ID;   break;
    case BuiltinType::Float:      ID = pch::PREDEF_TYPE_FLOAT_ID;      break;
    case BuiltinType::Double:     ID = pch::PREDEF_TYPE_DOUBLE_ID;     break;
    case BuiltinType::LongDouble: ID = pch::PREDEF_TYPE_LONGDOUBLE_ID; break;
    case BuiltinType::Overload:   ID = pch::PREDEF_TYPE_OVERLOAD_ID;   break;
    case BuiltinType::Dependent:  ID = pch::PREDEF_TYPE_DEPENDENT_ID;  break;
    }

    Record.push_back((ID << 3) | T.getCVRQualifiers());
    return;
  }

  pch::ID &ID = TypeIDs[T.getTypePtr()];
  if (ID == 0) // we haven't seen this type before
    ID = NextTypeID++;

  // Encode the type qualifiers in the type reference.
  Record.push_back((ID << 3) | T.getCVRQualifiers());
}

void PCHWriter::AddDeclRef(const Decl *D, RecordData &Record) {
  if (D == 0) {
    Record.push_back(0);
    return;
  }

  pch::ID &ID = DeclIDs[D];
  if (ID == 0) { 
    // We haven't seen this declaration before. Give it a new ID and
    // enqueue it in the list of declarations to emit.
    ID = DeclIDs.size();
    DeclsToEmit.push(const_cast<Decl *>(D));
  }

  Record.push_back(ID);
}

void PCHWriter::AddDeclarationName(DeclarationName Name, RecordData &Record) {
  Record.push_back(Name.getNameKind());
  switch (Name.getNameKind()) {
  case DeclarationName::Identifier:
    AddIdentifierRef(Name.getAsIdentifierInfo(), Record);
    break;

  case DeclarationName::ObjCZeroArgSelector:
  case DeclarationName::ObjCOneArgSelector:
  case DeclarationName::ObjCMultiArgSelector:
    assert(false && "Serialization of Objective-C selectors unavailable");
    break;

  case DeclarationName::CXXConstructorName:
  case DeclarationName::CXXDestructorName:
  case DeclarationName::CXXConversionFunctionName:
    AddTypeRef(Name.getCXXNameType(), Record);
    break;

  case DeclarationName::CXXOperatorName:
    Record.push_back(Name.getCXXOverloadedOperator());
    break;

  case DeclarationName::CXXUsingDirective:
    // No extra data to emit
    break;
  }
}
