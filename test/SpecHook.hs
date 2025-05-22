{-# LANGUAGE OverloadedStrings #-}

module SpecHook where
-- | This module is a hook for Hspec to use the JUnit formatter.
-- | It is used to generate JUnit XML reports for the test suite.

import Test.Hspec
import Test.Hspec.JUnit.Config
import qualified Test.Hspec.JUnit.Formatter as Formatter

-- | The 'hook' function is used to configure the JUnit formatter for Hspec.
hook :: Spec -> Spec
hook = Formatter.add $ defaultJUnitConfig "generic-persistence-test"

