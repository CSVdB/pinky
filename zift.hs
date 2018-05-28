#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --resolver lts-10.0
    --nix
    --package zifter-0.0.1.8
    --package validity-0.4.0.4
    --package zifter-git
    --package zifter-hindent
    --package zifter-hlint-0.0.0.1
-}
import Zifter
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint

main :: IO ()
main =
    ziftWith $ do
        preprocessor hindentZift
        prechecker gitAddAllZift
        checker hlintZift
