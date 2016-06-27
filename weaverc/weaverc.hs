{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           System.Environment

import           Weaver


main = do
  [server, self] <- getArgs
  withWeaver server self $ \ctx -> do
    (msg :: Either String Weaver.Message) <- weaverCall ctx server "echo" $ Hello self
    print msg
