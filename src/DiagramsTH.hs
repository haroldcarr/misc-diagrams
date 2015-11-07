{-# LANGUAGE OverloadedStrings #-}

module DiagramsTH where

import           Language.Haskell.TH

type FunctionName = String
type DiagramLabel = String

mk :: [(FunctionName, DiagramLabel)] -> Q [Dec]
mk = return . map mkBinding
  where
    mkBinding (fName, label) =
        ValD (VarP (mkName fName))
             (NormalB (AppE (AppE (VarE (mkName "rectangle")) (LitE (StringL fName)))
                            (LitE (StringL label)))) []

