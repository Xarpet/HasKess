# HasKess
 A Haskell Chess AI/Engine. Compact, powerful,  and most importantly *Functional*.

Currently, HasKess supports:

- Chess Interface

  - Full chess play with FIDE compliant rule set
  - 64-bit board representation (Bitboard)

  - support for FEN (both parsing and showing)

  - support for reading algebraic notation with disambiguation

- AI:

  - Minimax search algorithm, with alpha-beta pruning.
  - Evaluation function with Efficiently Updatable Neural Network

To be done:
- Universal Chess Interface support
- Better optimization of Foreign Function Interfacing
- Quiescence Search
- Transposition table
- End game database

Acknowledgments:
A huge thank you to Daniel Shawul for his amazing tool helping integrating NNUE into HasKess
https://github.com/dshawul/nnue-probe
