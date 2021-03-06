module SaveLoad where
import Board
import Data.Binary

{-
Encodes / Decodes all of the data types in to binary
-}

instance Binary World where
   put (World f b c h go hp ai lm pvp) =  do put f
                                             put b
                                             put c
                                             put h
                                             put go
                                             put hp
                                             put ai
                                             put lm
                                             put pvp
   get = do f <- get
            b  <- get
            c  <- get
            h  <- get
            go <- get
            hp <- get
            ai <- get
            lm <- get
            pvp <- get
            return (World f b c h go hp ai lm pvp)



instance Binary Board  where
   put (Board s t p w _ l sc) = do  put s
                                    put t
                                    put p
                                    put w
                                    put l
                                    put sc
   get = do s         <- get
            t         <- get
            p         <- get
            w         <- get
            l         <- get
            sc        <- get
            return (Board s t p w Nothing l sc)

instance Binary Flags where
   put (Flags bs t h w p) = do put bs
                               put t
                               put h
                               put w
                               put p

   get = do bs <- get
            t <- get
            h <- get
            w <- get
            p <- get
            return (Flags bs t h w p)


instance Binary Col where
   put Black = put (1 :: Word8)
   put White = put (0 :: Word8)
   get = do c <- getWord8
            case c of
              1 -> return Black
              0 -> return White
