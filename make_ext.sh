./grail.native < in > llvm_out.ll && 
clang -o final llvm_out.ll ./external/disp.c

mv final bin

