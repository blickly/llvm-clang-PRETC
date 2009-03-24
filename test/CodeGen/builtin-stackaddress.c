// RUN: clang-cc -emit-llvm < %s | grep "llvm.returnaddress" &&
// RUN: clang-cc -emit-llvm < %s | grep "llvm.frameaddress"
void* a(unsigned x) {
return __builtin_return_address(0);
}

void* c(unsigned x) {
return __builtin_frame_address(0);
}
