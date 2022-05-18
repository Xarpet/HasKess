ghc-8.10.7 -dynamic Main.hs -o main -lnnueprobe -L. -optl-Wl,-rpath,'$ORIGIN' -fprof-auto

# ghci -lnnueprobe
# note that you need .so and .nnue at /usr/lib or any other directory that ld looks at. 
# Use ld -l*libname* --verbose

