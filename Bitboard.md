We are using Rank-File bitboard mapping. So we start with a1 b1 c1 and work our way to h8

- Northwest : +7
- North: +8
- Northeast: +9
- West: -1
- East: +1
- Southeast: -9
- South: -8
- Southwest: -7

And some hex constants:

```pseudocode
a-file             0x0101010101010101
h-file             0x8080808080808080
1st rank           0x00000000000000FF
8th rank           0xFF00000000000000
a1-h8 diagonal     0x8040201008040201
h1-a8 antidiagonal 0x0102040810204080
light squares      0x55AA55AA55AA55AA
dark squares       0xAA55AA55AA55AA55
```