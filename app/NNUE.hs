{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
module NNUE where

import Foreign.C.String ( newCString, CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import qualified Data.Vector.Storable as VS

foreign import ccall unsafe "nnue_init" initialize :: CString -> CInt
foreign import ccall unsafe "nnue_evaluate_fen" fenEval :: CString -> CInt
foreign import ccall unsafe "nnue_evaluate" evalBoard :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- input :: (ForeignPtr a, Int, Int)
-- input = unsafeToForeignPtr

arrayEval :: CInt -> VS.Vector CInt -> VS.Vector CInt -> IO CInt
arrayEval color v1 v2 = 
    VS.unsafeWith v1 $ \p ->
        VS.unsafeWith v2 $ \s ->
            evalBoard color p s
    -- Explanation: Lambda inline to enable two vectors. UnsafeWith free the pointer when the function is done.
    -- When use: you need unsafePerformIO to get the Int.
{-# INLINE arrayEval #-}


initializeNNUE :: IO ()
initializeNNUE = do
    name <- newCString "nn-c3ca321c51c9.nnue"
    print $ initialize name