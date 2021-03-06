// RUN: clang-cc -fsyntax-only -verify %s 

void f() {
  float v1 = float(1);
  int v2 = typeof(int)(1,2); // expected-error {{function-style cast to a builtin type can only take one argument}}
  typedef int arr[];
  int v3 = arr(); // expected-error {{array types cannot be value-initialized}}
  int v4 = int();
  int v5 = int; // expected-error {{expected '(' for function-style cast or type construction}}
  typedef int T;
  int *p;
  bool v6 = T(0) == p;
  char *str;
  str = "a string";
  wchar_t *wstr;
  wstr = L"a wide string";
}
