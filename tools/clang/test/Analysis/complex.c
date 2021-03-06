// RUN: clang-cc -analyze -checker-simple -analyzer-store=basic -analyzer-constraints=basic -verify %s &&
// RUN: clang-cc -analyze -checker-cfref -analyzer-store=basic -analyzer-constraints=basic -verify %s &&
// RUN: clang-cc -analyze -checker-cfref -analyzer-store=basic -analyzer-constraints=range -verify %s &&
// RUN: clang-cc -analyze -checker-cfref -analyzer-store=region -analyzer-constraints=basic -verify %s &&
// RUN: clang-cc -analyze -checker-cfref -analyzer-store=region -analyzer-constraints=range -verify %s

#include <stdint.h>

int f1(int * p) {
  
  // This branch should be infeasible
  // because __imag__ p is 0.
  if (!p && __imag__ (intptr_t) p)
    *p = 1; // no-warning

  // If p != 0 then this branch is feasible; otherwise it is not.
  if (__real__ (intptr_t) p)
    *p = 1; // no-warning
    
  *p = 2; // expected-warning{{Dereference of null pointer}}
}
