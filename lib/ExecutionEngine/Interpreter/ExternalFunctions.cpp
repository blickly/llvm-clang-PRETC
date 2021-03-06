//===-- ExternalFunctions.cpp - Implement External Functions --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file contains both code to deal with invoking "external" functions, but
//  also contains code that implements "exported" external functions.
//
//  There are currently two mechanisms for handling external functions in the
//  Interpreter.  The first is to implement lle_* wrapper functions that are
//  specific to well-known library functions which manually translate the
//  arguments from GenericValues and make the call.  If such a wrapper does
//  not exist, and libffi is available, then the Interpreter will attempt to
//  invoke the function using libffi, after finding its address.
//
//===----------------------------------------------------------------------===//

#include "Interpreter.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/Config/config.h"     // Detect libffi
#include "llvm/Support/Streams.h"
#include "llvm/System/DynamicLibrary.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Support/ManagedStatic.h"
#include <csignal>
#include <cstdio>
#include <map>
#include <cmath>
#include <cstring>

#ifdef HAVE_FFI_CALL
#ifdef HAVE_FFI_H
#include <ffi.h>
#define USE_LIBFFI
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#define USE_LIBFFI
#endif
#endif

using namespace llvm;

typedef GenericValue (*ExFunc)(const FunctionType *,
                               const std::vector<GenericValue> &);
static ManagedStatic<std::map<const Function *, ExFunc> > ExportedFunctions;
static std::map<std::string, ExFunc> FuncNames;

#ifdef USE_LIBFFI
typedef void (*RawFunc)(void);
static ManagedStatic<std::map<const Function *, RawFunc> > RawFunctions;
#endif

static Interpreter *TheInterpreter;

static char getTypeID(const Type *Ty) {
  switch (Ty->getTypeID()) {
  case Type::VoidTyID:    return 'V';
  case Type::IntegerTyID:
    switch (cast<IntegerType>(Ty)->getBitWidth()) {
      case 1:  return 'o';
      case 8:  return 'B';
      case 16: return 'S';
      case 32: return 'I';
      case 64: return 'L';
      default: return 'N';
    }
  case Type::FloatTyID:   return 'F';
  case Type::DoubleTyID:  return 'D';
  case Type::PointerTyID: return 'P';
  case Type::FunctionTyID:return 'M';
  case Type::StructTyID:  return 'T';
  case Type::ArrayTyID:   return 'A';
  case Type::OpaqueTyID:  return 'O';
  default: return 'U';
  }
}

// Try to find address of external function given a Function object.
// Please note, that interpreter doesn't know how to assemble a
// real call in general case (this is JIT job), that's why it assumes,
// that all external functions has the same (and pretty "general") signature.
// The typical example of such functions are "lle_X_" ones.
static ExFunc lookupFunction(const Function *F) {
  // Function not found, look it up... start by figuring out what the
  // composite function name should be.
  std::string ExtName = "lle_";
  const FunctionType *FT = F->getFunctionType();
  for (unsigned i = 0, e = FT->getNumContainedTypes(); i != e; ++i)
    ExtName += getTypeID(FT->getContainedType(i));
  ExtName += "_" + F->getName();

  ExFunc FnPtr = FuncNames[ExtName];
  if (FnPtr == 0)
    FnPtr = FuncNames["lle_X_"+F->getName()];
  if (FnPtr == 0)  // Try calling a generic function... if it exists...
    FnPtr = (ExFunc)(intptr_t)sys::DynamicLibrary::SearchForAddressOfSymbol(
            ("lle_X_"+F->getName()).c_str());
  if (FnPtr != 0)
    ExportedFunctions->insert(std::make_pair(F, FnPtr));  // Cache for later
  return FnPtr;
}

#ifdef USE_LIBFFI
static ffi_type *ffiTypeFor(const Type *Ty) {
  switch (Ty->getTypeID()) {
    case Type::VoidTyID: return &ffi_type_void;
    case Type::IntegerTyID:
      switch (cast<IntegerType>(Ty)->getBitWidth()) {
        case 8:  return &ffi_type_sint8;
        case 16: return &ffi_type_sint16;
        case 32: return &ffi_type_sint32;
        case 64: return &ffi_type_sint64;
      }
    case Type::FloatTyID:   return &ffi_type_float;
    case Type::DoubleTyID:  return &ffi_type_double;
    case Type::PointerTyID: return &ffi_type_pointer;
    default: break;
  }
  // TODO: Support other types such as StructTyID, ArrayTyID, OpaqueTyID, etc.
  cerr << "Type could not be mapped for use with libffi.\n";
  abort();
  return NULL;
}

static void *ffiValueFor(const Type *Ty, const GenericValue &AV,
                         void *ArgDataPtr) {
  switch (Ty->getTypeID()) {
    case Type::IntegerTyID:
      switch (cast<IntegerType>(Ty)->getBitWidth()) {
        case 8: {
          int8_t *I8Ptr = (int8_t *) ArgDataPtr;
          *I8Ptr = (int8_t) AV.IntVal.getZExtValue();
          return ArgDataPtr;
        }
        case 16: {
          int16_t *I16Ptr = (int16_t *) ArgDataPtr;
          *I16Ptr = (int16_t) AV.IntVal.getZExtValue();
          return ArgDataPtr;
        }
        case 32: {
          int32_t *I32Ptr = (int32_t *) ArgDataPtr;
          *I32Ptr = (int32_t) AV.IntVal.getZExtValue();
          return ArgDataPtr;
        }
        case 64: {
          int64_t *I64Ptr = (int64_t *) ArgDataPtr;
          *I64Ptr = (int64_t) AV.IntVal.getZExtValue();
          return ArgDataPtr;
        }
      }
    case Type::FloatTyID: {
      float *FloatPtr = (float *) ArgDataPtr;
      *FloatPtr = AV.DoubleVal;
      return ArgDataPtr;
    }
    case Type::DoubleTyID: {
      double *DoublePtr = (double *) ArgDataPtr;
      *DoublePtr = AV.DoubleVal;
      return ArgDataPtr;
    }
    case Type::PointerTyID: {
      void **PtrPtr = (void **) ArgDataPtr;
      *PtrPtr = GVTOP(AV);
      return ArgDataPtr;
    }
    default: break;
  }
  // TODO: Support other types such as StructTyID, ArrayTyID, OpaqueTyID, etc.
  cerr << "Type value could not be mapped for use with libffi.\n";
  abort();
  return NULL;
}

static bool ffiInvoke(RawFunc Fn, Function *F,
                      const std::vector<GenericValue> &ArgVals,
                      const TargetData *TD, GenericValue &Result) {
  ffi_cif cif;
  const FunctionType *FTy = F->getFunctionType();
  const unsigned NumArgs = F->arg_size();

  // TODO: We don't have type information about the remaining arguments, because
  // this information is never passed into ExecutionEngine::runFunction().
  if (ArgVals.size() > NumArgs && F->isVarArg()) {
    cerr << "Calling external var arg function '" << F->getName()
         << "' is not supported by the Interpreter.\n";
    abort();
  }

  unsigned ArgBytes = 0;

  std::vector<ffi_type*> args(NumArgs);
  for (Function::const_arg_iterator A = F->arg_begin(), E = F->arg_end();
       A != E; ++A) {
    const unsigned ArgNo = A->getArgNo();
    const Type *ArgTy = FTy->getParamType(ArgNo);
    args[ArgNo] = ffiTypeFor(ArgTy);
    ArgBytes += TD->getTypeStoreSize(ArgTy);
  }

  uint8_t *ArgData = (uint8_t*) alloca(ArgBytes);
  uint8_t *ArgDataPtr = ArgData;
  std::vector<void*> values(NumArgs);
  for (Function::const_arg_iterator A = F->arg_begin(), E = F->arg_end();
       A != E; ++A) {
    const unsigned ArgNo = A->getArgNo();
    const Type *ArgTy = FTy->getParamType(ArgNo);
    values[ArgNo] = ffiValueFor(ArgTy, ArgVals[ArgNo], ArgDataPtr);
    ArgDataPtr += TD->getTypeStoreSize(ArgTy);
  }

  const Type *RetTy = FTy->getReturnType();
  ffi_type *rtype = ffiTypeFor(RetTy);

  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, NumArgs, rtype, &args[0]) == FFI_OK) {
    void *ret = NULL;
    if (RetTy->getTypeID() != Type::VoidTyID)
      ret = alloca(TD->getTypeStoreSize(RetTy));
    ffi_call(&cif, Fn, ret, &values[0]);
    switch (RetTy->getTypeID()) {
      case Type::IntegerTyID:
        switch (cast<IntegerType>(RetTy)->getBitWidth()) {
          case 8:  Result.IntVal = APInt(8 , *(int8_t *) ret); break;
          case 16: Result.IntVal = APInt(16, *(int16_t*) ret); break;
          case 32: Result.IntVal = APInt(32, *(int32_t*) ret); break;
          case 64: Result.IntVal = APInt(64, *(int64_t*) ret); break;
        }
        break;
      case Type::FloatTyID:   Result.FloatVal   = *(float *) ret; break;
      case Type::DoubleTyID:  Result.DoubleVal  = *(double*) ret; break;
      case Type::PointerTyID: Result.PointerVal = *(void **) ret; break;
      default: break;
    }
    return true;
  }

  return false;
}
#endif // USE_LIBFFI

GenericValue Interpreter::callExternalFunction(Function *F,
                                     const std::vector<GenericValue> &ArgVals) {
  TheInterpreter = this;

  // Do a lookup to see if the function is in our cache... this should just be a
  // deferred annotation!
  std::map<const Function *, ExFunc>::iterator FI = ExportedFunctions->find(F);
  if (ExFunc Fn = (FI == ExportedFunctions->end()) ? lookupFunction(F)
                                                   : FI->second)
    return Fn(F->getFunctionType(), ArgVals);

#ifdef USE_LIBFFI
  std::map<const Function *, RawFunc>::iterator RF = RawFunctions->find(F);
  RawFunc RawFn;
  if (RF == RawFunctions->end()) {
    RawFn = (RawFunc)(intptr_t)
      sys::DynamicLibrary::SearchForAddressOfSymbol(F->getName());
    if (RawFn != 0)
      RawFunctions->insert(std::make_pair(F, RawFn));  // Cache for later
  } else {
    RawFn = RF->second;
  }

  GenericValue Result;
  if (RawFn != 0 && ffiInvoke(RawFn, F, ArgVals, getTargetData(), Result))
    return Result;
#endif // USE_LIBFFI

  cerr << "Tried to execute an unknown external function: "
       << F->getType()->getDescription() << " " << F->getName() << "\n";
  if (F->getName() != "__main")
    abort();
  return GenericValue();
}


//===----------------------------------------------------------------------===//
//  Functions "exported" to the running application...
//
extern "C" {  // Don't add C++ manglings to llvm mangling :)

// void atexit(Function*)
GenericValue lle_X_atexit(const FunctionType *FT,
                          const std::vector<GenericValue> &Args) {
  assert(Args.size() == 1);
  TheInterpreter->addAtExitHandler((Function*)GVTOP(Args[0]));
  GenericValue GV;
  GV.IntVal = 0;
  return GV;
}

// void exit(int)
GenericValue lle_X_exit(const FunctionType *FT,
                        const std::vector<GenericValue> &Args) {
  TheInterpreter->exitCalled(Args[0]);
  return GenericValue();
}

// void abort(void)
GenericValue lle_X_abort(const FunctionType *FT,
                         const std::vector<GenericValue> &Args) {
  raise (SIGABRT);
  return GenericValue();
}

// int sprintf(char *, const char *, ...) - a very rough implementation to make
// output useful.
GenericValue lle_X_sprintf(const FunctionType *FT,
                           const std::vector<GenericValue> &Args) {
  char *OutputBuffer = (char *)GVTOP(Args[0]);
  const char *FmtStr = (const char *)GVTOP(Args[1]);
  unsigned ArgNo = 2;

  // printf should return # chars printed.  This is completely incorrect, but
  // close enough for now.
  GenericValue GV; 
  GV.IntVal = APInt(32, strlen(FmtStr));
  while (1) {
    switch (*FmtStr) {
    case 0: return GV;             // Null terminator...
    default:                       // Normal nonspecial character
      sprintf(OutputBuffer++, "%c", *FmtStr++);
      break;
    case '\\': {                   // Handle escape codes
      sprintf(OutputBuffer, "%c%c", *FmtStr, *(FmtStr+1));
      FmtStr += 2; OutputBuffer += 2;
      break;
    }
    case '%': {                    // Handle format specifiers
      char FmtBuf[100] = "", Buffer[1000] = "";
      char *FB = FmtBuf;
      *FB++ = *FmtStr++;
      char Last = *FB++ = *FmtStr++;
      unsigned HowLong = 0;
      while (Last != 'c' && Last != 'd' && Last != 'i' && Last != 'u' &&
             Last != 'o' && Last != 'x' && Last != 'X' && Last != 'e' &&
             Last != 'E' && Last != 'g' && Last != 'G' && Last != 'f' &&
             Last != 'p' && Last != 's' && Last != '%') {
        if (Last == 'l' || Last == 'L') HowLong++;  // Keep track of l's
        Last = *FB++ = *FmtStr++;
      }
      *FB = 0;

      switch (Last) {
      case '%':
        strcpy(Buffer, "%"); break;
      case 'c':
        sprintf(Buffer, FmtBuf, uint32_t(Args[ArgNo++].IntVal.getZExtValue()));
        break;
      case 'd': case 'i':
      case 'u': case 'o':
      case 'x': case 'X':
        if (HowLong >= 1) {
          if (HowLong == 1 &&
              TheInterpreter->getTargetData()->getPointerSizeInBits() == 64 &&
              sizeof(long) < sizeof(int64_t)) {
            // Make sure we use %lld with a 64 bit argument because we might be
            // compiling LLI on a 32 bit compiler.
            unsigned Size = strlen(FmtBuf);
            FmtBuf[Size] = FmtBuf[Size-1];
            FmtBuf[Size+1] = 0;
            FmtBuf[Size-1] = 'l';
          }
          sprintf(Buffer, FmtBuf, Args[ArgNo++].IntVal.getZExtValue());
        } else
          sprintf(Buffer, FmtBuf,uint32_t(Args[ArgNo++].IntVal.getZExtValue()));
        break;
      case 'e': case 'E': case 'g': case 'G': case 'f':
        sprintf(Buffer, FmtBuf, Args[ArgNo++].DoubleVal); break;
      case 'p':
        sprintf(Buffer, FmtBuf, (void*)GVTOP(Args[ArgNo++])); break;
      case 's':
        sprintf(Buffer, FmtBuf, (char*)GVTOP(Args[ArgNo++])); break;
      default:  cerr << "<unknown printf code '" << *FmtStr << "'!>";
        ArgNo++; break;
      }
      strcpy(OutputBuffer, Buffer);
      OutputBuffer += strlen(Buffer);
      }
      break;
    }
  }
  return GV;
}

// int printf(const char *, ...) - a very rough implementation to make output
// useful.
GenericValue lle_X_printf(const FunctionType *FT,
                          const std::vector<GenericValue> &Args) {
  char Buffer[10000];
  std::vector<GenericValue> NewArgs;
  NewArgs.push_back(PTOGV((void*)&Buffer[0]));
  NewArgs.insert(NewArgs.end(), Args.begin(), Args.end());
  GenericValue GV = lle_X_sprintf(FT, NewArgs);
  cout << Buffer;
  return GV;
}

static void ByteswapSCANFResults(const char *Fmt, void *Arg0, void *Arg1,
                                 void *Arg2, void *Arg3, void *Arg4, void *Arg5,
                                 void *Arg6, void *Arg7, void *Arg8) {
  void *Args[] = { Arg0, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7, Arg8, 0 };

  // Loop over the format string, munging read values as appropriate (performs
  // byteswaps as necessary).
  unsigned ArgNo = 0;
  while (*Fmt) {
    if (*Fmt++ == '%') {
      // Read any flag characters that may be present...
      bool Suppress = false;
      bool Half = false;
      bool Long = false;
      bool LongLong = false;  // long long or long double

      while (1) {
        switch (*Fmt++) {
        case '*': Suppress = true; break;
        case 'a': /*Allocate = true;*/ break;  // We don't need to track this
        case 'h': Half = true; break;
        case 'l': Long = true; break;
        case 'q':
        case 'L': LongLong = true; break;
        default:
          if (Fmt[-1] > '9' || Fmt[-1] < '0')   // Ignore field width specs
            goto Out;
        }
      }
    Out:

      // Read the conversion character
      if (!Suppress && Fmt[-1] != '%') { // Nothing to do?
        unsigned Size = 0;
        const Type *Ty = 0;

        switch (Fmt[-1]) {
        case 'i': case 'o': case 'u': case 'x': case 'X': case 'n': case 'p':
        case 'd':
          if (Long || LongLong) {
            Size = 8; Ty = Type::Int64Ty;
          } else if (Half) {
            Size = 4; Ty = Type::Int16Ty;
          } else {
            Size = 4; Ty = Type::Int32Ty;
          }
          break;

        case 'e': case 'g': case 'E':
        case 'f':
          if (Long || LongLong) {
            Size = 8; Ty = Type::DoubleTy;
          } else {
            Size = 4; Ty = Type::FloatTy;
          }
          break;

        case 's': case 'c': case '[':  // No byteswap needed
          Size = 1;
          Ty = Type::Int8Ty;
          break;

        default: break;
        }

        if (Size) {
          GenericValue GV;
          void *Arg = Args[ArgNo++];
          memcpy(&GV, Arg, Size);
          TheInterpreter->StoreValueToMemory(GV, (GenericValue*)Arg, Ty);
        }
      }
    }
  }
}

// int sscanf(const char *format, ...);
GenericValue lle_X_sscanf(const FunctionType *FT,
                          const std::vector<GenericValue> &args) {
  assert(args.size() < 10 && "Only handle up to 10 args to sscanf right now!");

  char *Args[10];
  for (unsigned i = 0; i < args.size(); ++i)
    Args[i] = (char*)GVTOP(args[i]);

  GenericValue GV;
  GV.IntVal = APInt(32, sscanf(Args[0], Args[1], Args[2], Args[3], Args[4],
                        Args[5], Args[6], Args[7], Args[8], Args[9]));
  ByteswapSCANFResults(Args[1], Args[2], Args[3], Args[4],
                       Args[5], Args[6], Args[7], Args[8], Args[9], 0);
  return GV;
}

// int scanf(const char *format, ...);
GenericValue lle_X_scanf(const FunctionType *FT,
                         const std::vector<GenericValue> &args) {
  assert(args.size() < 10 && "Only handle up to 10 args to scanf right now!");

  char *Args[10];
  for (unsigned i = 0; i < args.size(); ++i)
    Args[i] = (char*)GVTOP(args[i]);

  GenericValue GV;
  GV.IntVal = APInt(32, scanf( Args[0], Args[1], Args[2], Args[3], Args[4],
                        Args[5], Args[6], Args[7], Args[8], Args[9]));
  ByteswapSCANFResults(Args[0], Args[1], Args[2], Args[3], Args[4],
                       Args[5], Args[6], Args[7], Args[8], Args[9]);
  return GV;
}

// int fprintf(FILE *, const char *, ...) - a very rough implementation to make
// output useful.
GenericValue lle_X_fprintf(const FunctionType *FT,
                           const std::vector<GenericValue> &Args) {
  assert(Args.size() >= 2);
  char Buffer[10000];
  std::vector<GenericValue> NewArgs;
  NewArgs.push_back(PTOGV(Buffer));
  NewArgs.insert(NewArgs.end(), Args.begin()+1, Args.end());
  GenericValue GV = lle_X_sprintf(FT, NewArgs);

  fputs(Buffer, (FILE *) GVTOP(Args[0]));
  return GV;
}

} // End extern "C"


void Interpreter::initializeExternalFunctions() {
  FuncNames["lle_X_atexit"]       = lle_X_atexit;
  FuncNames["lle_X_exit"]         = lle_X_exit;
  FuncNames["lle_X_abort"]        = lle_X_abort;

  FuncNames["lle_X_printf"]       = lle_X_printf;
  FuncNames["lle_X_sprintf"]      = lle_X_sprintf;
  FuncNames["lle_X_sscanf"]       = lle_X_sscanf;
  FuncNames["lle_X_scanf"]        = lle_X_scanf;
  FuncNames["lle_X_fprintf"]      = lle_X_fprintf;
}

