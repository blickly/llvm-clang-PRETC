// RUN: clang-cc -fmath-errno=1 -emit-llvm -o %t %s &&
// RUN: grep "declare " %t | count 6 &&
// RUN: grep "declare " %t | grep "@llvm." | count 1 &&
// RUN: clang-cc -fmath-errno=0 -emit-llvm -o %t %s &&
// RUN: grep "declare " %t | count 6 &&
// RUN: grep "declare " %t | grep -v "@llvm." | count 0

// IRgen only pays attention to const; it should always call llvm for
// this.
float sqrtf(float) __attribute__((const));

void test_sqrt(float a0, double a1, long double a2) {
  float l0 = sqrtf(a0);
  double l1 = sqrt(a1);
  long double l2 = sqrtl(a2);
}

void test_pow(float a0, double a1, long double a2) {
  float l0 = powf(a0, a0);
  double l1 = pow(a1, a1);
  long double l2 = powl(a2, a2);
}
