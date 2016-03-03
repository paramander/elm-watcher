module Compile where

import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Process (readProcessWithExitCode)

data CommandError
    = MissingExe String
    | CommandFailed String String

missingExe :: String -> CommandError
missingExe command =
   MissingExe $
     "Could not find command `" ++ command ++ "`. Do you have it installed?\n\
     \    Can it be run from anywhere? Is it on your PATH?"

unwrappedRun :: String -> [String] -> IO (Either CommandError String)
unwrappedRun command args =
  do  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""
      return $
          case exitCode of
            ExitSuccess ->
              Right stdout

            ExitFailure code ->
              if code == 127 then
                Left (missingExe command)  -- UNIX

              else if code == 9009 then
                Left (missingExe command)  -- Windows

              else
                Left (CommandFailed stdout stderr)

compile :: FilePath -> FilePath -> IO (Either String String)
compile source output =
  do result <- unwrappedRun "elm-make" [ "--yes", source, ("--output=" ++ output) ]
     case result of
       Left (MissingExe msg) ->
         return (Left msg)

       Left (CommandFailed out err) ->
         return (Left (out ++ err))

       Right _ ->
         do code <- readFile output
            return (Right code)
