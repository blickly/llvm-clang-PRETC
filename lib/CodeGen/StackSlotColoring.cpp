//===-- StackSlotColoring.cpp - Stack slot coloring pass. -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the stack slot coloring pass.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "stackcoloring"
#include "VirtRegMap.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/LiveStackAnalysis.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include <vector>
using namespace llvm;

static cl::opt<bool>
DisableSharing("no-stack-slot-sharing",
             cl::init(false), cl::Hidden,
             cl::desc("Suppress slot sharing during stack coloring"));

static cl::opt<bool>
ColorWithRegs("color-ss-with-regs",
             cl::init(false), cl::Hidden,
             cl::desc("Color stack slots with free registers"));


static cl::opt<int> DCELimit("ssc-dce-limit", cl::init(-1), cl::Hidden);

STATISTIC(NumEliminated, "Number of stack slots eliminated due to coloring");
STATISTIC(NumDead,       "Number of trivially dead stack accesses eliminated");
STATISTIC(NumRegRepl,    "Number of stack slot refs replaced with reg refs");

namespace {
  class VISIBILITY_HIDDEN StackSlotColoring : public MachineFunctionPass {
    LiveStacks* LS;
    VirtRegMap* VRM;
    MachineFrameInfo *MFI;
    MachineRegisterInfo *MRI;
    const TargetInstrInfo  *TII;
    const TargetRegisterInfo *TRI;
    const MachineLoopInfo *loopInfo;

    // SSIntervals - Spill slot intervals.
    std::vector<LiveInterval*> SSIntervals;

    // SSRefs - Keep a list of frame index references for each spill slot.
    SmallVector<SmallVector<MachineInstr*, 8>, 16> SSRefs;

    // OrigAlignments - Alignments of stack objects before coloring.
    SmallVector<unsigned, 16> OrigAlignments;

    // OrigSizes - Sizess of stack objects before coloring.
    SmallVector<unsigned, 16> OrigSizes;

    // AllColors - If index is set, it's a spill slot, i.e. color.
    // FIXME: This assumes PEI locate spill slot with smaller indices
    // closest to stack pointer / frame pointer. Therefore, smaller
    // index == better color.
    BitVector AllColors;

    // NextColor - Next "color" that's not yet used.
    int NextColor;

    // UsedColors - "Colors" that have been assigned.
    BitVector UsedColors;

    // Assignments - Color to intervals mapping.
    SmallVector<SmallVector<LiveInterval*,4>, 16> Assignments;

  public:
    static char ID; // Pass identification
    StackSlotColoring() : MachineFunctionPass(&ID), NextColor(-1) {}
    
    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<LiveStacks>();
      AU.addRequired<VirtRegMap>();
      AU.addPreserved<VirtRegMap>();      
      AU.addRequired<MachineLoopInfo>();
      AU.addPreserved<MachineLoopInfo>();
      AU.addPreservedID(MachineDominatorsID);
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    virtual bool runOnMachineFunction(MachineFunction &MF);
    virtual const char* getPassName() const {
      return "Stack Slot Coloring";
    }

  private:
    void InitializeSlots();
    void ScanForSpillSlotRefs(MachineFunction &MF);
    bool OverlapWithAssignments(LiveInterval *li, int Color) const;
    int ColorSlot(LiveInterval *li);
    bool ColorSlots(MachineFunction &MF);
    bool ColorSlotsWithFreeRegs(SmallVector<int, 16> &SlotMapping,
                                SmallVector<SmallVector<int, 4>, 16> &RevMap,
                                BitVector &SlotIsReg);
    void RewriteInstruction(MachineInstr *MI, int OldFI, int NewFI,
                            MachineFunction &MF);
    void UnfoldAndRewriteInstruction(MachineInstr *MI, int OldFI,
                                     unsigned Reg, MachineFunction &MF);
    bool AllMemRefsCanBeUnfolded(int SS);
    bool RemoveDeadStores(MachineBasicBlock* MBB);
  };
} // end anonymous namespace

char StackSlotColoring::ID = 0;

static RegisterPass<StackSlotColoring>
X("stack-slot-coloring", "Stack Slot Coloring");

FunctionPass *llvm::createStackSlotColoringPass() {
  return new StackSlotColoring();
}

namespace {
  // IntervalSorter - Comparison predicate that sort live intervals by
  // their weight.
  struct IntervalSorter {
    bool operator()(LiveInterval* LHS, LiveInterval* RHS) const {
      return LHS->weight > RHS->weight;
    }
  };
}

/// ScanForSpillSlotRefs - Scan all the machine instructions for spill slot
/// references and update spill slot weights.
void StackSlotColoring::ScanForSpillSlotRefs(MachineFunction &MF) {
  SSRefs.resize(MFI->getObjectIndexEnd());

  // FIXME: Need the equivalent of MachineRegisterInfo for frameindex operands.
  for (MachineFunction::iterator MBBI = MF.begin(), E = MF.end();
       MBBI != E; ++MBBI) {
    MachineBasicBlock *MBB = &*MBBI;
    unsigned loopDepth = loopInfo->getLoopDepth(MBB);
    for (MachineBasicBlock::iterator MII = MBB->begin(), EE = MBB->end();
         MII != EE; ++MII) {
      MachineInstr *MI = &*MII;
      for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
        MachineOperand &MO = MI->getOperand(i);
        if (!MO.isFI())
          continue;
        int FI = MO.getIndex();
        if (FI < 0)
          continue;
        if (!LS->hasInterval(FI))
          continue;
        LiveInterval &li = LS->getInterval(FI);
        li.weight += LiveIntervals::getSpillWeight(false, true, loopDepth);
        SSRefs[FI].push_back(MI);
      }
    }
  }
}

/// InitializeSlots - Process all spill stack slot liveintervals and add them
/// to a sorted (by weight) list.
void StackSlotColoring::InitializeSlots() {
  int LastFI = MFI->getObjectIndexEnd();
  OrigAlignments.resize(LastFI);
  OrigSizes.resize(LastFI);
  AllColors.resize(LastFI);
  UsedColors.resize(LastFI);
  Assignments.resize(LastFI);

  // Gather all spill slots into a list.
  DOUT << "Spill slot intervals:\n";
  for (LiveStacks::iterator i = LS->begin(), e = LS->end(); i != e; ++i) {
    LiveInterval &li = i->second;
    DEBUG(li.dump());
    int FI = li.getStackSlotIndex();
    if (MFI->isDeadObjectIndex(FI))
      continue;
    SSIntervals.push_back(&li);
    OrigAlignments[FI] = MFI->getObjectAlignment(FI);
    OrigSizes[FI]      = MFI->getObjectSize(FI);
    AllColors.set(FI);
  }
  DOUT << '\n';

  // Sort them by weight.
  std::stable_sort(SSIntervals.begin(), SSIntervals.end(), IntervalSorter());

  // Get first "color".
  NextColor = AllColors.find_first();
}

/// OverlapWithAssignments - Return true if LiveInterval overlaps with any
/// LiveIntervals that have already been assigned to the specified color.
bool
StackSlotColoring::OverlapWithAssignments(LiveInterval *li, int Color) const {
  const SmallVector<LiveInterval*,4> &OtherLIs = Assignments[Color];
  for (unsigned i = 0, e = OtherLIs.size(); i != e; ++i) {
    LiveInterval *OtherLI = OtherLIs[i];
    if (OtherLI->overlaps(*li))
      return true;
  }
  return false;
}

/// ColorSlotsWithFreeRegs - If there are any free registers available, try
/// replacing spill slots references with registers instead.
bool
StackSlotColoring::ColorSlotsWithFreeRegs(SmallVector<int, 16> &SlotMapping,
                                   SmallVector<SmallVector<int, 4>, 16> &RevMap,
                                   BitVector &SlotIsReg) {
  if (!ColorWithRegs || !VRM->HasUnusedRegisters())
    return false;

  bool Changed = false;
  DOUT << "Assigning unused registers to spill slots:\n";
  for (unsigned i = 0, e = SSIntervals.size(); i != e; ++i) {
    LiveInterval *li = SSIntervals[i];
    int SS = li->getStackSlotIndex();
    if (!UsedColors[SS])
      continue;
    // Get the largest common sub- register class of all the stack slots that
    // are colored to this stack slot.
    const TargetRegisterClass *RC = 0;
    for (unsigned j = 0, ee = RevMap[SS].size(); j != ee; ++j) {
      int RSS = RevMap[SS][j];
      const TargetRegisterClass *RRC = LS->getIntervalRegClass(RSS);
      if (!RC)
        RC = RRC;
      else
        RC = getCommonSubClass(RC, RRC);
    }

    // If it's not colored to another stack slot, try coloring it
    // to a "free" register.
    if (!RC)
      continue;
    unsigned Reg = VRM->getFirstUnusedRegister(RC);
    if (!Reg)
      continue;
    bool IsSafe = true;
    for (unsigned j = 0, ee = RevMap[SS].size(); j != ee; ++j) {
      int RSS = RevMap[SS][j];
      if (!AllMemRefsCanBeUnfolded(RSS)) {
        IsSafe = false;
        break;
      }
    }
    if (!IsSafe)
      // Try color the next spill slot.
      continue;

    DOUT << "Assigning fi#" << SS << " to " << TRI->getName(Reg)
         << ", which in turn means...\n";
    // Register and its sub-registers are no longer free.
    VRM->setRegisterUsed(Reg);
    // If reg is a callee-saved register, it will have to be spilled in
    // the prologue.
    MRI->setPhysRegUsed(Reg);
    for (const unsigned *AS = TRI->getAliasSet(Reg); *AS; ++AS) {
      VRM->setRegisterUsed(*AS);
      MRI->setPhysRegUsed(*AS);
    }
    // This spill slot is dead after the rewrites
    MFI->RemoveStackObject(SS);

    // Remember all these FI references will have to be unfolded.
    for (unsigned j = 0, ee = RevMap[SS].size(); j != ee; ++j) {
      int RSS = RevMap[SS][j];
      DOUT << "  Assigning fi#" << RSS << " to " << TRI->getName(Reg) << '\n';
      SlotMapping[RSS] = Reg;
      SlotIsReg.set(RSS);
    }

    ++NumEliminated;
    Changed = true;
  }
  DOUT << '\n';

  return Changed;
}

/// ColorSlot - Assign a "color" (stack slot) to the specified stack slot.
///
int StackSlotColoring::ColorSlot(LiveInterval *li) {
  int Color = -1;
  bool Share = false;
  if (!DisableSharing) {
    // Check if it's possible to reuse any of the used colors.
    Color = UsedColors.find_first();
    while (Color != -1) {
      if (!OverlapWithAssignments(li, Color)) {
        Share = true;
        ++NumEliminated;
        break;
      }
      Color = UsedColors.find_next(Color);
    }
  }

  // Assign it to the first available color (assumed to be the best) if it's
  // not possible to share a used color with other objects.
  if (!Share) {
    assert(NextColor != -1 && "No more spill slots?");
    Color = NextColor;
    UsedColors.set(Color);
    NextColor = AllColors.find_next(NextColor);
  }

  // Record the assignment.
  Assignments[Color].push_back(li);
  int FI = li->getStackSlotIndex();
  DOUT << "Assigning fi#" << FI << " to fi#" << Color << "\n";

  // Change size and alignment of the allocated slot. If there are multiple
  // objects sharing the same slot, then make sure the size and alignment
  // are large enough for all.
  unsigned Align = OrigAlignments[FI];
  if (!Share || Align > MFI->getObjectAlignment(Color))
    MFI->setObjectAlignment(Color, Align);
  int64_t Size = OrigSizes[FI];
  if (!Share || Size > MFI->getObjectSize(Color))
    MFI->setObjectSize(Color, Size);
  return Color;
}

/// Colorslots - Color all spill stack slots and rewrite all frameindex machine
/// operands in the function.
bool StackSlotColoring::ColorSlots(MachineFunction &MF) {
  unsigned NumObjs = MFI->getObjectIndexEnd();
  SmallVector<int, 16> SlotMapping(NumObjs, -1);
  SmallVector<float, 16> SlotWeights(NumObjs, 0.0);
  SmallVector<SmallVector<int, 4>, 16> RevMap(NumObjs);
  BitVector SlotIsReg(NumObjs);
  BitVector UsedColors(NumObjs);

  DOUT << "Color spill slot intervals:\n";
  bool Changed = false;
  for (unsigned i = 0, e = SSIntervals.size(); i != e; ++i) {
    LiveInterval *li = SSIntervals[i];
    int SS = li->getStackSlotIndex();
    int NewSS = ColorSlot(li);
    assert(NewSS >= 0 && "Stack coloring failed?");
    SlotMapping[SS] = NewSS;
    RevMap[NewSS].push_back(SS);
    SlotWeights[NewSS] += li->weight;
    UsedColors.set(NewSS);
    Changed |= (SS != NewSS);
  }

  DOUT << "\nSpill slots after coloring:\n";
  for (unsigned i = 0, e = SSIntervals.size(); i != e; ++i) {
    LiveInterval *li = SSIntervals[i];
    int SS = li->getStackSlotIndex();
    li->weight = SlotWeights[SS];
  }
  // Sort them by new weight.
  std::stable_sort(SSIntervals.begin(), SSIntervals.end(), IntervalSorter());

#ifndef NDEBUG
  for (unsigned i = 0, e = SSIntervals.size(); i != e; ++i)
    DEBUG(SSIntervals[i]->dump());
  DOUT << '\n';
#endif

  // Can we "color" a stack slot with a unused register?
  Changed |= ColorSlotsWithFreeRegs(SlotMapping, RevMap, SlotIsReg);

  if (!Changed)
    return false;

  // Rewrite all MO_FrameIndex operands.
  for (unsigned SS = 0, SE = SSRefs.size(); SS != SE; ++SS) {
    bool isReg = SlotIsReg[SS];
    int NewFI = SlotMapping[SS];
    if (NewFI == -1 || (NewFI == (int)SS && !isReg))
      continue;

    SmallVector<MachineInstr*, 8> &RefMIs = SSRefs[SS];
    for (unsigned i = 0, e = RefMIs.size(); i != e; ++i)
      if (isReg)
        // Rewrite to use a register instead.
        UnfoldAndRewriteInstruction(RefMIs[i], SS, NewFI, MF);
      else
        RewriteInstruction(RefMIs[i], SS, NewFI, MF);
  }

  // Delete unused stack slots.
  while (NextColor != -1) {
    DOUT << "Removing unused stack object fi#" << NextColor << "\n";
    MFI->RemoveStackObject(NextColor);
    NextColor = AllColors.find_next(NextColor);
  }

  return true;
}

/// AllMemRefsCanBeUnfolded - Return true if all references of the specified
/// spill slot index can be unfolded.
bool StackSlotColoring::AllMemRefsCanBeUnfolded(int SS) {
  SmallVector<MachineInstr*, 8> &RefMIs = SSRefs[SS];
  for (unsigned i = 0, e = RefMIs.size(); i != e; ++i) {
    MachineInstr *MI = RefMIs[i];
    if (!TII->getOpcodeAfterMemoryUnfold(MI->getOpcode(), false, false))
      return false;
    for (unsigned j = 0, ee = MI->getNumOperands(); j != ee; ++j) {
      MachineOperand &MO = MI->getOperand(j);
      if (MO.isFI() && MO.getIndex() != SS)
        // If it uses another frameindex, we can, currently* unfold it.
        return false;
    }
  }
  return true;
}

/// RewriteInstruction - Rewrite specified instruction by replacing references
/// to old frame index with new one.
void StackSlotColoring::RewriteInstruction(MachineInstr *MI, int OldFI,
                                           int NewFI, MachineFunction &MF) {
  for (unsigned i = 0, ee = MI->getNumOperands(); i != ee; ++i) {
    MachineOperand &MO = MI->getOperand(i);
    if (!MO.isFI())
      continue;
    int FI = MO.getIndex();
    if (FI != OldFI)
      continue;
    MO.setIndex(NewFI);
  }

  // Update the MachineMemOperand for the new memory location.
  // FIXME: We need a better method of managing these too.
  SmallVector<MachineMemOperand, 2> MMOs(MI->memoperands_begin(),
                                         MI->memoperands_end());
  MI->clearMemOperands(MF);
  const Value *OldSV = PseudoSourceValue::getFixedStack(OldFI);
  for (unsigned i = 0, ee = MMOs.size(); i != ee; ++i) {
    if (MMOs[i].getValue() != OldSV)
      MI->addMemOperand(MF, MMOs[i]);
    else {
      MachineMemOperand MMO(PseudoSourceValue::getFixedStack(NewFI),
                            MMOs[i].getFlags(), MMOs[i].getOffset(),
                            MMOs[i].getSize(),  MMOs[i].getAlignment());
      MI->addMemOperand(MF, MMO);
    }
  }
}

/// UnfoldAndRewriteInstruction - Rewrite specified instruction by unfolding
/// folded memory references and replacing those references with register
/// references instead.
void StackSlotColoring::UnfoldAndRewriteInstruction(MachineInstr *MI, int OldFI,
                                                    unsigned Reg,
                                                    MachineFunction &MF) {
  MachineBasicBlock *MBB = MI->getParent();
  SmallVector<MachineInstr*, 4> NewMIs;
  bool Success = TII->unfoldMemoryOperand(MF, MI, Reg, false, false, NewMIs);
  assert(Success && "Failed to unfold!");
  MBB->insert(MI, NewMIs[0]);
  MBB->erase(MI);
  ++NumRegRepl;
}

/// RemoveDeadStores - Scan through a basic block and look for loads followed
/// by stores.  If they're both using the same stack slot, then the store is
/// definitely dead.  This could obviously be much more aggressive (consider
/// pairs with instructions between them), but such extensions might have a
/// considerable compile time impact.
bool StackSlotColoring::RemoveDeadStores(MachineBasicBlock* MBB) {
  // FIXME: This could be much more aggressive, but we need to investigate
  // the compile time impact of doing so.
  bool changed = false;

  SmallVector<MachineInstr*, 4> toErase;

  for (MachineBasicBlock::iterator I = MBB->begin(), E = MBB->end();
       I != E; ++I) {
    if (DCELimit != -1 && (int)NumDead >= DCELimit)
      break;
    
    MachineBasicBlock::iterator NextMI = next(I);
    if (NextMI == MBB->end()) continue;
    
    int FirstSS, SecondSS;
    unsigned LoadReg = 0;
    unsigned StoreReg = 0;
    if (!(LoadReg = TII->isLoadFromStackSlot(I, FirstSS))) continue;
    if (!(StoreReg = TII->isStoreToStackSlot(NextMI, SecondSS))) continue;
    if (FirstSS != SecondSS || LoadReg != StoreReg || FirstSS == -1) continue;
    
    ++NumDead;
    changed = true;
    
    if (NextMI->findRegisterUseOperandIdx(LoadReg, true, 0) != -1) {
      ++NumDead;
      toErase.push_back(I);
    }
    
    toErase.push_back(NextMI);
    ++I;
  }
  
  for (SmallVector<MachineInstr*, 4>::iterator I = toErase.begin(),
       E = toErase.end(); I != E; ++I)
    (*I)->eraseFromParent();
  
  return changed;
}


bool StackSlotColoring::runOnMachineFunction(MachineFunction &MF) {
  DOUT << "********** Stack Slot Coloring **********\n";

  MFI = MF.getFrameInfo();
  MRI = &MF.getRegInfo(); 
  TII = MF.getTarget().getInstrInfo();
  TRI = MF.getTarget().getRegisterInfo();
  LS = &getAnalysis<LiveStacks>();
  VRM = &getAnalysis<VirtRegMap>();
  loopInfo = &getAnalysis<MachineLoopInfo>();

  bool Changed = false;

  unsigned NumSlots = LS->getNumIntervals();
  if (NumSlots < 2) {
    if (NumSlots == 0 || !VRM->HasUnusedRegisters())
      // Nothing to do!
      return false;
  }

  // Gather spill slot references
  ScanForSpillSlotRefs(MF);
  InitializeSlots();
  Changed = ColorSlots(MF);

  NextColor = -1;
  SSIntervals.clear();
  for (unsigned i = 0, e = SSRefs.size(); i != e; ++i)
    SSRefs[i].clear();
  SSRefs.clear();
  OrigAlignments.clear();
  OrigSizes.clear();
  AllColors.clear();
  UsedColors.clear();
  for (unsigned i = 0, e = Assignments.size(); i != e; ++i)
    Assignments[i].clear();
  Assignments.clear();

  if (Changed) {
    for (MachineFunction::iterator I = MF.begin(), E = MF.end(); I != E; ++I)
      Changed |= RemoveDeadStores(I);
  }

  return Changed;
}
