module Compile where

import qualified Elm.Utils as Utils

compile :: FilePath -> FilePath -> IO (Either String String)
compile source output =
  do result <- Utils.unwrappedRun "elm-make" [ "--yes", source, ("--output=" ++ output) ]
     case result of
       Left (Utils.MissingExe msg) ->
         return (Left msg)

       Left (Utils.CommandFailed out err) ->
         return (Left (out ++ err))

       Right _ ->
         do code <- readFile output
            return (Right code)
