-- this compiler pragma allows GHC to automatically discover all Hspec Test Specs.
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- Avoid warning for missing export list in test/Spec.hs
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
