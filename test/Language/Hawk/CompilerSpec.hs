module Language.Hawk.CompilerSpec (main, spec) where

import Test.Hspec
import Language.Hawk.Compile
import Language.Hawk.Compile.Config

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hawk compiler" $ do
    it "can compile code" $ do
      hkc $ HkcConfig
            { _hkcSrcFiles = ["example/Example.hk", "example/Fib.hk"]
            , _hkcOutFile = ""
            , _hkcProd = undefined
            , _hkcExAst = []
            , _hkcExLib = []
            , _hkcOpts = undefined
            }
      
