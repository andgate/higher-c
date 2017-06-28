{-# LANGUAGE OverloadedStrings #-}
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Data.Default.Class
import Language.Hawk.Compile
import Language.Hawk.Compile.Options

main :: IO ()
main = do
    test <- testSpec "hawk" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $
    it "Can compile" $ do
        let conf =
              HkcConfig
                { _hkcSrcFiles  = ["example/Fib.hk", "example/Example.hk"]
                , _hkcOutFile   = "example/example.out"
                , _hkcProd      = Bin
                , _hkcExAst     = [] -- List of serialized ast files (hkast)
                , _hkcExLib     = [] -- List of library files (.dll or .so)
                , _hkcOpts      = def
                }
        hkc conf