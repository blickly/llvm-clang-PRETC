//===-- ARMSubtarget.cpp - ARM Subtarget Information ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the ARM specific subclass of TargetSubtarget.
//
//===----------------------------------------------------------------------===//

#include "ARMSubtarget.h"
#include "ARMGenSubtarget.inc"
#include "llvm/Module.h"
using namespace llvm;

ARMSubtarget::ARMSubtarget(const Module &M, const std::string &FS,
                           bool isThumb)
  : ARMArchVersion(V4T)
  , ARMFPUType(None)
  , IsThumb(isThumb)
  , ThumbMode(Thumb1)
  , UseThumbBacktraces(false)
  , IsR9Reserved(false)
  , stackAlignment(4)
  , CPUString("generic")
  , TargetType(isELF) // Default to ELF unless otherwise specified.
  , TargetABI(ARM_ABI_APCS) {
  // Determine default and user specified characteristics

  // Parse features string.
  CPUString = ParseSubtargetFeatures(FS, CPUString);

  // Set the boolean corresponding to the current target triple, or the default
  // if one cannot be determined, to true.
  const std::string& TT = M.getTargetTriple();
  unsigned Len = TT.length();
  unsigned Idx = 0;

  if (Len >= 5 && TT.substr(0, 4) == "armv")
    Idx = 4;
  else if (Len >= 6 && TT.substr(0, 6) == "thumb") {
    IsThumb = true;
    if (Len >= 7 && TT[5] == 'v')
      Idx = 6;
  }
  if (Idx) {
    unsigned SubVer = TT[Idx];
    if (SubVer > '4' && SubVer <= '9') {
      if (SubVer >= '7')
        ARMArchVersion = V7A;
      else if (SubVer == '6')
        ARMArchVersion = V6;
      else if (SubVer == '5') {
        ARMArchVersion = V5T;
        if (Len >= Idx+3 && TT[Idx+1] == 't' && TT[Idx+2] == 'e')
          ARMArchVersion = V5TE;
      }
    }
  }

  if (Len >= 10) {
    if (TT.find("-darwin") != std::string::npos)
      // arm-darwin
      TargetType = isDarwin;
  } else if (TT.empty()) {
#if defined(__APPLE__)
    TargetType = isDarwin;
#endif
  }

  if (TT.find("eabi") != std::string::npos)
    TargetABI = ARM_ABI_AAPCS;

  if (isAAPCS_ABI())
    stackAlignment = 8;

  if (isTargetDarwin()) {
    UseThumbBacktraces = true;
    IsR9Reserved = true;
  }
}
