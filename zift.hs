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
-}
import Zifter
import Zifter.Git
import Zifter.Hindent

main :: IO ()
main =
    ziftWith $ do
        preprocessor hindentZift
        prechecker gitAddAllZift
        checker $ pure ()
