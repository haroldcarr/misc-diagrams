{-# LANGUAGE OverloadedStrings #-}

module DiagramsTH where

import           Language.Haskell.TH

type FunctionName = String
type DiagramLabel = String

mk :: String -> [(FunctionName, DiagramLabel)] -> Q [Dec]
mk fname = return . map mkBinding
  where
    mkBinding (fName, label) =
        ValD (VarP (mkName fName))
             (NormalB (AppE (AppE (VarE (mkName fname)) (LitE (StringL fName)))
                            (LitE (StringL label)))) []

