module SaveLoad where
import Board
import Data.Binary

{-
Encodes / Decodes all of the data types in to binary
-}

instance Binary World where
   put (World b c h go hp ai lm) =  do put b
                                       put c
                                       put h
                                       put go
                                       put hp
                                       put ai
                                       put lm
   get = do b  <- get
            c  <- get
            h  <- get
            go <- get
            hp <- get
            ai <- get
            lm <- get
            return (World b c h go hp ai lm)



instance Binary Board  where
   put (Board s t p w _) = do  put s
                               put t
                               put p
                               put w


   get = do s       <- get
            t       <- get
            p       <- get
            w       <- get
            return (Board s t p w Nothing)

instance Binary Flags where
   put (Flags h w) = do put h
                        put w

   get = do h <- get
            w <- get
            return (Flags h w)


instance Binary Col where
   put Black = do put (1 :: Word8)
   put White = do put (0 :: Word8)
   get = do c <- getWord8
            case c of
              1 -> return Black
              0 -> return White
