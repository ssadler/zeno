{-# LANGUAGE TemplateHaskell #-}

module Zeno.Version where


import GitHash
import Paths_zeno (version)
import Data.Version (showVersion)
import Text.Printf


revisionInfo :: String
revisionInfo =
  printf "zeno %s %s@%s %s (commit %s) %s"
    (showVersion version)
    (giBranch gi)
    (take 8 $ giHash gi)
    (take 6 $ drop 4 $ giCommitDate gi)
    (show $ giCommitCount gi)
    (if giDirty gi then "(dirty)" else "" :: String)
  where
  gi = $$tGitInfoCwd
