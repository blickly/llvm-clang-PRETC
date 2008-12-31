// RUN: clang -fsyntax-only -verify %s

struct X {
  int& f(int) const; // expected-note{{candidate function}}
  float& f(int); // expected-note{{candidate function}}

  void test_f(int x) const {
    int& i = f(x);
  }

  void test_f2(int x) {
    float& f2 = f(x);
  }

  int& g(int) const; // expected-note{{candidate function}}
  float& g(int); // expected-note{{candidate function}}
  static double& g(double); // expected-note{{candidate function}}

  void h(int);
};

void test(X x, const X xc, X* xp, const X* xcp, volatile X xv, volatile X* xvp) {
  int& i1 = xc.f(0);
  int& i2 = xcp->f(0);
  float& f1 = x.f(0);
  float& f2 = xp->f(0);
  xv.f(0); // expected-error{{no matching member function for call to 'f'; candidates are:}}
  xvp->f(0); // expected-error{{no matching member function for call to 'f'; candidates are:}}

  int& i3 = xc.g(0);
  int& i4 = xcp->g(0);
  float& f3 = x.g(0);
  float& f4 = xp->g(0);
  double& d1 = xp->g(0.0);
  double& d2 = X::g(0.0);
  X::g(0); // expected-error{{call to 'g' is ambiguous; candidates are:}}
  
  X::h(0); // expected-error{{call to non-static member function without an object argument}}
}
