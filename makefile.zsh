# ghc-8.10.7 -dynamic Main.hs -o main -lnnueprobe -L. -optl-Wl,-rpath,'$ORIGIN' -fprof-auto
# to run, cabal run haskess -- +RTS -Nx -A300M
# ghci -lnnueprobe
# note you need to pass the current directory to ld in order to link
# Use ld -L. -l*libname* --verbose

# to make the movegen C file
# cd moveGen
# gcc -fPIC -c *.c
# gcc -shared *.o -o libtest.so
# cp libtest.so /usr/lib/
# cp libtest.so ../
# cd ..
# cabal run

cp libtest.so /usr/lib/
cp libnnueprobe.so /usr/lib
cabal run

