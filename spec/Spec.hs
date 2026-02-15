module Main where

import qualified LibSpec
import Test.Hspec

main :: IO ()
main = hspec LibSpec.spec