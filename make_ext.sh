./grail.native < graphtest2.gl > llvm_out.ll && 
clang -o final llvm_out.ll ./external/disp.c

mv final bin
cd bin
./final
