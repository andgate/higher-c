{-# Language TemplateHaskell
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , LambdaCase
           , FlexibleContexts
  #-}
module Main where

import Prelude hiding (lex)

import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Loops (untilM_)
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Bitraversable
import Data.Either
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Alonzo.Analysis
import Language.Alonzo.Analysis.Error
import Language.Alonzo.Lex.Error
import Language.Alonzo.Parse.Error
import Language.Alonzo.Syntax.Location
import Language.Alonzo.Transform
import Language.Alonzo.Transform.Reduce (reduce)
import System.IO (hFlush, stdout)
import System.Exit
import System.Console.Repline

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Language.Alonzo.Parse              as P
import qualified Language.Alonzo.Transform.ANorm    as A
import qualified Language.Alonzo.Transform.NameBind as B
import qualified Language.Alonzo.Analysis.NameCheck as N
import qualified Language.Alonzo.Syntax.Source      as S

import qualified Data.Text.IO as T

-- Maybe delete this?
data ReplError
  = ReplParseErr
  | ReplAnalysisErr
  | ReplReductionErr
  | ReplFileLoadErr FilePath
  | ReplSeriousErr Text -- Rly srs


instance Pretty ReplError where
    pretty = \case
        ReplParseErr    -> "Repl aborted."
        ReplAnalysisErr -> "Repl aborted."
        ReplAnalysisErr -> "Repl aborted."
        ReplFileLoadErr fp -> pretty fp <+> ": Failed to load."
        ReplSeriousErr err -> pretty err


data ReplState
  = ReplState
    { _replDict     :: Map Text Loc
    , _replFiles    :: Map FilePath S.Closure
    , _replPrograms :: Map Text A.Term
    }


makeLenses ''ReplState

instance Semigroup ReplState where
  (<>) _ _ = error "i don't want this"


instance Monoid ReplState where
  mempty =
    ReplState
    { _replDict = mempty
    , _replFiles = mempty
    , _replPrograms = mempty
    }

main :: IO ()
main = repl

------------------------------------------------------------------------
-- Read-Evaluate-Print-Loop

type Repl a = HaskelineT (StateT ReplState IO) a

repl :: IO ()
repl = flip evalStateT mempty
     $ evalRepl ">>> " cmd opts (Word comp) enter

-- Start-up
enter :: Repl ()
enter = do
  loadPrelude
  liftIO $ putStrLn "Hello!"

-- Evalution
cmd :: String -> Repl ()
cmd input = do
  cl <- S.closure <$> parseText "" (pack input)
  --printPretty cl
  -- Validate programs and terms
  checkClosure cl
  -- Store programs
  -- evalute terms

{-
  case s of
    S.Program n t -> saveProgram n t
    S.ExecTerm t  -> evalTerm t >>= printPretty
-}



-- Completion
comp :: (Monad m, MonadState ReplState m) => WordCompleter m
comp n = do
  ns <- use replDict
  return $ List.filter (List.isPrefixOf n) (unpack <$> Map.keys ns)


-- Commands
quitRepl :: [String] -> Repl ()
quitRepl _ = liftIO $ exitWith ExitSuccess

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", loadFiles) -- :load <files>
  , ("quit", quitRepl) -- :quit
  ]


------------------------------------------------------------------------
-- Parsing

parseFiles :: [(FilePath, Text)] -> Repl [[S.Stmt]]
parseFiles fs = do
  case P.parseFiles fs of
    ([], ss) -> return ss
    (es, _)  -> do
      printPretties es
      abort

parseText :: FilePath -> Text -> Repl [S.Stmt]
parseText fp src = do
  case P.parseText fp src of
    ([], ss) -> return ss
    (es, _)  -> do
      printPretty es
      abort


------------------------------------------------------------------------
-- Files

loadFiles :: [FilePath] -> Repl ()
loadFiles [] = liftIO $ print "No files given."
loadFiles paths = do
  srcs <- liftIO $ traverse T.readFile paths
  ss <- parseFiles $ zip paths srcs
  let cs = S.closure <$> ss
      loaded = zip paths cs
      c@(S.Closure ps ts) = mconcat cs
  checkClosure c

  storeClosures $ zip paths cs

  reduceTerms ts



------------------------------------------------------------------------
-- Checking

checkClosure :: S.Closure -> Repl ()
checkClosure c = do
  d <- replDictWith c
  checkNameConflicts d
  checkNames (Map.fromList d) c


replDictWith :: S.Closure -> Repl [(Text, Loc)]
replDictWith (S.Closure ps ts) = do
  d <- Map.toList <$> use replDict
  return $ d ++ map S._progName ps


checkNameConflicts :: [(Text, Loc)] -> Repl ()
checkNameConflicts ns =
  case N.checkConflicts ns of
    [] -> return ()
    ers -> do
      printPretties ers
      abort


checkNames :: Map Text Loc -> S.Closure -> Repl ()
checkNames d (S.Closure ps ts) = do
  let ts' = map S._progTerm ps ++ ts
      ers = N.namecheckTerms d ts'
  case ers of
    [] -> return ()
    _ -> do
      printPretties ers
      abort


------------------------------------------------------------------------
-- Program Storage

storeClosures :: [(FilePath, S.Closure)] -> Repl ()
storeClosures cls = do
  let (S.Closure ps _) = mconcat . map snd $ cls
      ps' = [(n, transformTerm t) | (S.Program (n, _) t) <- ps]

  replDict %= Map.union (Map.fromList $ S._progName <$> ps)
  replFiles %= Map.union (Map.fromList cls)
  replPrograms %= Map.union (Map.fromList ps')



------------------------------------------------------------------------
-- Term Transformation

transformTerm :: S.Term -> A.Term 
transformTerm t = undefined

------------------------------------------------------------------------
-- Term Reduction

reduceTerms :: [A.Term] -> Repl ()
reduceTerms ts = undefined

      

------------------------------------------------------------------------
-- Helpers

loadPrelude :: Repl ()
loadPrelude = loadFiles ["prelude/Prelude.al"]


printPretty :: Pretty p => p -> Repl ()
printPretty p =
  liftIO $ putDoc (pretty p) >> putStr "\n"

printPretties :: Pretty p => [p] -> Repl ()
printPretties ps =
  liftIO $ putDoc (vsep $ pretty <$> ps) >> putStr "\n"


{-
loopRepl :: Repl ()
loopRepl = do
  minput <- getInputLine "repl > "
  case minput of
      Nothing        -> return () -- probably a serious error
      Just ":quit"   -> return ()
      Just (':':cmd) -> handleCmd cmd >> loopRepl
      Just input     -> evalString    >> loopRepl





handleProcess :: Repl ()
handleProcess = catchError process handle
  where handle = \case
          ReplParseErr     -> return ()
          ReplAnalysisErr  -> return ()
          ReplReductionErr -> return ()
          ReplSeriousErr e -> throwError $ ReplSeriousErr e





checkStmt :: S.Stmt -> Repl ()
checkStmt st = do
  ns <- (Map.keys . _replClosure) <$> get
  case st of
    S.Program n t -> namecheck' (n:ns) t
    S.ExecTerm t -> namecheck' ns t

namecheck' :: [Text] -> S.Term -> Repl S.Term
namecheck' ns t =
  case namecheck (Set.fromList ns) (S.locOf t) t of
    [] -> return t
    es -> do
      let errMsg = vcat (pretty <$> es) 
      liftIO $ putDoc errMsg >> T.putStr "\n"
      throwError ReplAnalysisErr


saveProgram :: Text -> S.Term -> Repl ()
saveProgram n t = do
  ns <- uses replClosure Map.keys
  t' <- transform' =<< namecheck' (n:ns) t
  replClosure %= Map.insert n t'


evalTerm :: S.Term -> Repl A.Val
evalTerm t = do
  cl <- (Map.keys . _replClosure) <$> get
  namecheck' cl t >>= transform' >>= reduce'



transform' :: S.Term -> Repl A.Term
transform' t = do 
  liftIO $ putStr "Source:"
  printPretty t
  return $ transformANorm t

reduce' :: A.Term -> Repl A.Val
reduce' t = do
  liftIO $ putStr "Anormalized:"
  printPretty t
  cl <- _replClosure <$> get
  return $ reduce cl t
  




loadPrelude :: Repl ()
loadPrelude = loadFile "prelude/Prelude.al"


loadFile :: FilePath -> Repl ()
loadFile fp = (loadFile' fp) `catchError` handle
  where
    handle (ReplSeriousErr msg) = error (unpack msg)
    handle e = liftIO . putDoc . pretty $ e


loadFile' :: FilePath -> Repl ()
loadFile' fp = do
  src <- liftIO $ T.readFile fp
  case parseFile fp src of
    Left e   -> fileParseFail fp e
    Right ds -> do
      cl <- buildFile fp ds
      liftIO $ putStr ("Loaded: " ++ fp ++ "\n")


buildFile :: FilePath -> [S.Stmt] -> Repl A.Closure
buildFile fp ss = do
  let ps@(cl, ts) = S.closure ss
  -- checkPrograms fp ps
  installPrograms cl
  runPrograms ts


checkPrograms :: FilePath -> S.Closure -> Repl ()
checkPrograms fp ps@(cl, ts) = do
  detectClosureConflict fp cl
  replCl <- use replClosure
  let ns = Map.keys replCl ++ Map.keys cl
  -- TODO: Actually check the prelude names
  return ()


installPrograms :: S.Closure -> Repl ()
installPrograms cl = do
  let ts = Map.elems cl
  ts' <- mapM transform' ts

  let ns = Map.keys cl
      cl' = Map.fromList (zip ns ts')
  replClosure %= Map.union cl'

  mapM_ (installed . unpack) ns


runPrograms :: [S.Term] -> Repl ()
runPrograms ts =
  -- TODO: Run programs specified in prelude
  return ()


nameErrors :: FilePath -> [NameError] -> Repl ()
nameErrors fp es = do
  liftIO $ do
    putDoc $ vsep (pretty <$> es)
    putStr "\n"
  throwError $ ReplFileLoadErr fp


closureConflict :: FilePath -> [Text] -> Repl ()
closureConflict fp ns = do
  liftIO $ do
    T.putStr "Conflicting names detected:\n"
    putDoc $ hsep (pretty <$> ns)
    putStr "\n"
  throwError $ ReplFileLoadErr fp


fileParseFail :: FilePath -> ParseError -> Repl ()
fileParseFail fp e =
  liftIO $ do
    putDoc (pretty e)
    putStr fp
    T.putStr "\nFailed to load "
    putStr (fp ++ "\n")


installed :: String -> Repl ()
installed n = liftIO $ putStr ("Installed: " ++ n ++ "\n")


mergeClosures :: S.Closure -> S.Closure -> Repl S.Closure
mergeClosures cl1 cl2 = undefined
  -- Check name conflicts
  -- Merge
  -- Check undefined names
  -- return validated, merged closure


detectClosureConflict :: FilePath -> S.Closure -> Repl ()
detectClosureConflict fp cl = do
  nsRepl <- Map.keys <$> use replClosure
  case Map.keys cl `List.intersect` nsRepl of
    [] -> return ()
    ns -> closureConflict fp ns -- print error, throw exception
-}