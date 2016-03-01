# elm-watcher

A simple file watcher for Elm source files to recompile and output it to a JS file.

## Reason

We needed a solution like this because we want our backend to be written in Haskell, and our frontend in Elm. Having to recompile/rebuild Elm AND Haskell would soon be a pain, so this package was developed.

It uses the `Compile.compile` method from `elm-compiler` but with an added output `FilePath`.

## Usage

To use this in your Haskell codebase, integrate it in your development main method:

```{Haskell}

-- src/backend/devel.hs

import qualified Watcher as EW -- import elm-watcher

main :: IO ()
main = do
    let elmWatcherConfig = EW.WatchConfig { watchDir = "src/frontend"
                                          , compileFile = "Main.elm"
                                          , outputDir = "static/app.js"
                                          }
    forkIO $ EW.watchWithConfig elmWatcherConfig
    -- rest of your code, like spawning a warp server...
```
