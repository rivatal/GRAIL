
./grail.native < pete.gl > out.ll && 
clang -o final out.ll ./external/disp.c -lm

mv final bin
cd bin
./final
