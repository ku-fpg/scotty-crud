How to build
------------

Set up a shared sandbox.

    $ cabal sandbox init
    $ cabal install --only-dependencies 
    $ (cd tests    ; cabal sandbox init --sandbox  ../.cabal-sandbox )
    $ (cd examples ; cabal sandbox init --sandbox  ../.cabal-sandbox )
    $ (cd tools    ; cabal sandbox init --sandbox  ../.cabal-sandbox )
    

