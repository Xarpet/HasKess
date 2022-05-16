{-# LANGUAGE ForeignFunctionInterface #-}
module NNUE where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

foreign import ccall unsafe "nnue_init" initialize :: CString -> IO Int
foreign import ccall unsafe "nnue_evaluate_fen" eval :: CString -> IO Int

testeval :: IO ()
testeval = do
    location <- newCString "nn-eba324f53044.nnue"
    fen <- newCString "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    x <- initialize location
    y <- eval fen
    print y

