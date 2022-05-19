ghc-8.10.7 -dynamic Main.hs -o main -lnnueprobe -L. -optl-Wl,-rpath,'$ORIGIN' -fprof-auto
# to run, cabal run haskess -- +RTS -Nx -A300M
# ghci -lnnueprobe
# note you need to pass the current directory to ld in order to link
# Use ld -L. -l*libname* --verbose

