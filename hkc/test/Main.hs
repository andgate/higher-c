import Data.List
import Data.Proxy       (Proxy (..))
import Data.Traversable
import System.Directory (doesFileExist)
import System.FilePath  (takeDirectory, FilePath, (</>))
import Test.Tasty
import Test.Tasty.Golden  (findByExtension)
import Test.Tasty.Options (IsOption (..), OptionDescription (Option))
import Test.Tasty.Program (testProgram)

main :: IO ()
main = return ()