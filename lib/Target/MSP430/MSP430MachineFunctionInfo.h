//===- MSP430MachineFuctionInfo.h - MSP430 machine function info -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares MSP430-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef MSP430MACHINEFUNCTIONINFO_H
#define MSP430MACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

/// MSP430MachineFunctionInfo - This class is derived from MachineFunction and
/// contains private MSP430 target-specific information for each MachineFunction.
class MSP430MachineFunctionInfo : public MachineFunctionInfo {
  /// CalleeSavedFrameSize - Size of the callee-saved register portion of the
  /// stack frame in bytes.
  unsigned CalleeSavedFrameSize;

public:
  MSP430MachineFunctionInfo() : CalleeSavedFrameSize(0) {}

  MSP430MachineFunctionInfo(MachineFunction &MF) : CalleeSavedFrameSize(0) {}

  unsigned getCalleeSavedFrameSize() const { return CalleeSavedFrameSize; }
  void setCalleeSavedFrameSize(unsigned bytes) { CalleeSavedFrameSize = bytes; }
};

} // End llvm namespace

#endif
