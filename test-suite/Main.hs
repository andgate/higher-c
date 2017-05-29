-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import qualified Language.Hawk.Compile as C

main :: IO ()
main = do
    test <- testSpec "hawk" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "Can compile" $ do
        let pkg = C.Package "Example" "core"
            cSt = C.defState {C.cPkgs = [pkg]}
        C.compile cSt