if [ -n "$1" ]; then 
   FILE="$1"
else
   echo -n "File name is a required argument. Enter a .gl file. "
   exit
fi
./grail.native <"$FILE" > out.ll && 
clang -o final out.ll ./external/disp.c -lm

mv final bin/
cd bin
./final
