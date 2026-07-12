#!/usr/bin/env stack
{- stack script
   --resolver lts-24.34
   --package process
   --package directory
   --package filepath
   --package time
-}
{-# LANGUAGE LambdaCase #-}

-- Benchmark harness for the rzk typechecker. See bench/README.md.
--
--   bench/bench.hs run     [--quick] [--runs N] [--corpus NAME] [--rts "OPTS"] [--label STR]
--   bench/bench.hs profile [--quick] [--corpus NAME]
--   bench/bench.hs setup   [--corpus NAME]
--
-- The rzk binary is taken from $RZK_BIN if set, otherwise from the current
-- Stack build ("run"/"setup") or the current Stack profiling build ("profile").

import           Control.Monad      (forM, unless, when)
import           Data.Char          (isSpace)
import           Data.List          (isPrefixOf, isSuffixOf, sort)
import           Data.Maybe         (fromMaybe)
import           Data.Time          (defaultTimeLocale, formatTime,
                                     getCurrentTime)
import           System.Directory   (createDirectoryIfMissing, doesDirectoryExist,
                                     doesFileExist, listDirectory, renameFile)
import           System.Environment (getArgs, lookupEnv)
import           System.Exit        (ExitCode (..), exitFailure)
import           System.FilePath    ((</>))
import           System.IO          (hPutStrLn, stderr)
import           System.Process     (CreateProcess (..), proc,
                                     readCreateProcessWithExitCode, readProcess)

-- * Corpora

data Corpus = Corpus
  { corpusUrl    :: String
  , corpusCommit :: String
    -- | Files for --quick mode: directories with a file-name predicate,
    -- expanded in order (sorted within each directory). A prefix of the
    -- full project in dependency order.
  , corpusQuick  :: [(FilePath, String -> Bool)]
  }

corpora :: [(String, Corpus)]
corpora =
  [ ( "sHoTT"
    , Corpus
        { corpusUrl    = "https://github.com/rzk-lang/sHoTT.git"
        , corpusCommit = "5346e43807aad6a2dd17e645a8a6482b52b3cae3"
          -- up to and including 05-segal-types, the solver-heaviest file
        , corpusQuick  =
            [ ("src/hott", const True)
            , ("src/simplicial-hott", \name -> take 2 name `elem` ["02", "03", "04", "05"])
            ]
        }
    )
  ]

-- * Options

data Command = Setup | Run | Profile | Summary FilePath

data Options = Options
  { optQuick  :: Bool
  , optRuns   :: Int
  , optCorpus :: String
  , optRts    :: [String]
  , optLabel  :: String
  }

defaultOptions :: Options
defaultOptions = Options False 3 "sHoTT" [] ""

usage :: IO a
usage = do
  hPutStrLn stderr "usage: bench/bench.hs (setup | run | profile) \
                   \[--quick] [--runs N] [--corpus NAME] [--rts \"OPTS\"] [--label STR]\n\
                   \       bench/bench.hs summary FILE.prof"
  exitFailure

parseArgs :: [String] -> IO (Command, Options)
parseArgs [] = usage
parseArgs ["summary", file] = pure (Summary file, defaultOptions)
parseArgs (cmdStr : rest) = do
  cmd <- case cmdStr of
    "setup"   -> pure Setup
    "run"     -> pure Run
    "profile" -> pure Profile
    _         -> usage
  opts <- go defaultOptions rest
  pure (cmd, opts)
  where
    go opts []                     = pure opts
    go opts ("--quick" : xs)       = go opts { optQuick = True } xs
    go opts ("--runs" : n : xs)    = go opts { optRuns = read n } xs
    go opts ("--corpus" : c : xs)  = go opts { optCorpus = c } xs
    go opts ("--rts" : r : xs)     = go opts { optRts = words r } xs
    go opts ("--label" : l : xs)   = go opts { optLabel = l } xs
    go _ (arg : _)                 = do
      hPutStrLn stderr ("unknown option: " <> arg)
      usage

-- * Small process helpers

-- | Run a command in a directory; return (exit code, stdout <> stderr).
runIn :: FilePath -> String -> [String] -> IO (ExitCode, String)
runIn dir cmd args = do
  (code, out, err) <-
    readCreateProcessWithExitCode (proc cmd args) { cwd = Just dir } ""
  pure (code, out <> err)

-- | Run a command in a directory; die with the output on failure.
runIn_ :: FilePath -> String -> [String] -> IO ()
runIn_ dir cmd args = do
  (code, out) <- runIn dir cmd args
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      hPutStrLn stderr (cmd <> " " <> unwords args <> " failed:")
      hPutStrLn stderr (unlines (lastN 5 (lines out)))
      exitFailure

git :: FilePath -> [String] -> IO String
git dir args = trim <$> readProcess "git" (["-C", dir] ++ args) ""

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

-- * Paths and metadata

repoRoot :: IO FilePath
repoRoot = git "." ["rev-parse", "--show-toplevel"]

rzkBin :: FilePath -> Bool -> IO FilePath
rzkBin root profiling =
  lookupEnv "RZK_BIN" >>= \case
    Just bin -> pure bin
    Nothing -> do
      let pathArgs = ["path"] ++ ["--profile" | profiling] ++ ["--local-install-root"]
      (_, out, _) <- readCreateProcessWithExitCode
        (proc "stack" pathArgs) { cwd = Just root } ""
      pure (trim out </> "bin" </> "rzk")

rzkCommit :: FilePath -> IO String
rzkCommit root = do
  commit <- git root ["rev-parse", "--short=8", "HEAD"]
  dirty  <- git root ["status", "--porcelain", "--", "rzk/src", "rzk/grammar", "rzk/app"]
  pure (commit <> if null dirty then "" else "-dirty")

-- * Corpus management

lookupCorpus :: String -> IO Corpus
lookupCorpus name = case lookup name corpora of
  Just c  -> pure c
  Nothing -> do
    hPutStrLn stderr ("no such corpus: " <> name
      <> " (known: " <> unwords (map fst corpora) <> ")")
    exitFailure

ensureCorpus :: FilePath -> String -> Corpus -> IO FilePath
ensureCorpus benchDir name Corpus{corpusUrl = url, corpusCommit = commit} = do
  let dir = benchDir </> "corpora" </> name
  exists <- doesDirectoryExist (dir </> ".git")
  unless exists $ do
    putStrLn ("Cloning " <> name <> " from " <> url <> " ...")
    runIn_ "." "git" ["clone", "--quiet", url, dir]
  (haveCommit, _) <- runIn dir "git" ["cat-file", "-e", commit <> "^{commit}"]
  when (haveCommit /= ExitSuccess) $
    runIn_ dir "git" ["fetch", "--quiet", "origin"]
  runIn_ dir "git" ["checkout", "--quiet", "--detach", commit]
  status <- git dir ["status", "--porcelain"]
  unless (null status) $ do
    hPutStrLn stderr ("corpus checkout " <> dir <> " is not clean; refusing to run")
    exitFailure
  pure dir

quickFiles :: FilePath -> Corpus -> IO [FilePath]
quickFiles dir corpus =
  fmap concat . forM (corpusQuick corpus) $ \(subdir, keep) -> do
    names <- sort <$> listDirectory (dir </> subdir)
    pure [ subdir </> name | name <- names, ".rzk.md" `isSuffixOf` name, keep name ]

-- * Timed runs

-- | The @+RTS -t --machine-readable@ one-shot summary is a Haskell-readable
-- association list (after the leading command-line echo).
readStats :: FilePath -> IO [(String, String)]
readStats path = read . unlines . drop 1 . lines <$> readFile path

stat :: String -> [(String, String)] -> String
stat key stats = fromMaybe ("no such RTS stat: " <> key) (lookup key stats)

csvHeader :: String
csvHeader = "timestamp,host,rzk_commit,rzk_version,label,corpus,corpus_commit,\
            \mode,rts,run,wall_s,mut_wall_s,gc_wall_s,alloc_bytes,\
            \max_live_bytes,mem_in_use_bytes,num_gcs,productivity_wall"

benchRun :: FilePath -> FilePath -> Options -> IO ()
benchRun root benchDir opts = do
  corpus <- lookupCorpus (optCorpus opts)
  corpusDir <- ensureCorpus benchDir (optCorpus opts) corpus
  let resultsDir = benchDir </> "results"
      csv = resultsDir </> "results.csv"
  createDirectoryIfMissing True resultsDir
  bin <- rzkBin root False
  binOk <- doesFileExist bin
  unless binOk $ do
    hPutStrLn stderr ("rzk binary not found at " <> bin <> " (build first, or set RZK_BIN)")
    exitFailure
  version <- trim <$> readProcess bin ["version"] ""
  commit <- rzkCommit root
  files <- if optQuick opts then quickFiles corpusDir corpus else pure []
  csvExists <- doesFileExist csv
  unless csvExists $ writeFile csv (csvHeader <> "\n")

  let mode = if optQuick opts then "quick" else "full"
      rts  = if null (optRts opts) then "default" else unwords (optRts opts)
  putStrLn ("rzk " <> version <> " (" <> commit <> ") on " <> optCorpus opts
    <> "/" <> mode <> ", " <> show (optRuns opts) <> " run(s), RTS: " <> rts)

  walls <- forM [1 .. optRuns opts] $ \i -> do
    let statsFile = resultsDir </> ".stats.tmp"
        args = ["typecheck"] ++ files
            ++ ["+RTS"] ++ optRts opts
            ++ ["-t" <> statsFile, "--machine-readable", "-RTS"]
    (code, out) <- runIn corpusDir bin args
    case code of
      ExitSuccess -> pure ()
      ExitFailure _ -> do
        hPutStrLn stderr "typecheck FAILED; last lines of output:"
        hPutStrLn stderr (unlines (lastN 5 (lines out)))
        exitFailure
    stats <- readStats statsFile
    let field = (`stat` stats)
        row = [ field "total_wall_seconds", field "mut_wall_seconds"
              , field "GC_wall_seconds", field "allocated_bytes"
              , field "max_live_bytes", field "max_mem_in_use_bytes"
              , field "num_GCs", field "productivity_wall_percent" ]
    timestamp <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime
    host <- trim <$> readProcess "hostname" ["-s"] ""
    appendFile csv (commas
      ([ timestamp, host, commit, version, optLabel opts, optCorpus opts
       , corpusCommit corpus, mode, rts, show i ] ++ row) <> "\n")
    putStrLn ("  run " <> show i <> ": wall " <> field "total_wall_seconds"
      <> "s (mut " <> field "mut_wall_seconds" <> "s, gc " <> field "GC_wall_seconds"
      <> "s), alloc " <> field "allocated_bytes"
      <> ", max live " <> field "max_live_bytes")
    pure (read (field "total_wall_seconds") :: Double)

  putStrLn ("median wall: " <> show (median walls) <> "s   (appended to " <> csv <> ")")
  where
    commas = foldr1 (\x acc -> x <> "," <> acc)
    median xs =
      let ys = sort xs
          n  = length ys
      in if odd n then ys !! (n `div` 2)
                  else (ys !! (n `div` 2 - 1) + ys !! (n `div` 2)) / 2

-- * Profiled runs

benchProfile :: FilePath -> FilePath -> Options -> IO ()
benchProfile root benchDir opts = do
  corpus <- lookupCorpus (optCorpus opts)
  corpusDir <- ensureCorpus benchDir (optCorpus opts) corpus
  let resultsDir = benchDir </> "results"
  createDirectoryIfMissing True resultsDir
  external <- lookupEnv "RZK_BIN"
  bin <- case external of
    Just b  -> pure b
    Nothing -> do
      putStrLn "Building the profiling build (stack build --profile --ghc-options=-fprof-auto) ..."
      runIn_ root "stack" ["build", "--profile", "--ghc-options=-fprof-auto"]
      rzkBin root True
  commit <- rzkCommit root
  files <- if optQuick opts then quickFiles corpusDir corpus else pure []
  let mode = if optQuick opts then "quick" else "full"
      prof = resultsDir </> ("rzk-" <> commit <> "-" <> optCorpus opts <> "-" <> mode <> ".prof")
  putStrLn ("Profiled run on " <> optCorpus opts <> "/" <> mode
    <> " (this is a few times slower than a plain run) ...")
  (code, out) <- runIn corpusDir bin (["typecheck"] ++ files ++ ["+RTS", "-p", "-RTS"])
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      hPutStrLn stderr "profiled typecheck FAILED; last lines of output:"
      hPutStrLn stderr (unlines (lastN 5 (lines out)))
      exitFailure
  renameFile (corpusDir </> "rzk.prof") prof
  putStrLn ("profile written to " <> prof)
  profSummary prof

-- * Cost-centre profile summary

-- | Print the totals, the top flat cost centres, and the tope solver's
-- inherited share: the summed inherited %time of the outermost solver rows
-- in the call tree. Summing the flat (individual) %time instead
-- under-counts badly, because most of the solver's work is attributed to
-- the generic helpers it calls (term equality, substitution).
profSummary :: FilePath -> IO ()
profSummary path = do
  content <- lines <$> readFile path
  mapM_ (putStrLn . trim)
    [ l | l <- take 8 content, "total time" `isPrefixOf` trim l
                            || "total alloc" `isPrefixOf` trim l ]
  case [ i | (i, l) <- zip [0 ..] content, "COST CENTRE" `isPrefixOf` l ] of
    (flatHeader : treeHeader : _) -> do
      let topFlat = 20
          flatSection = takeWhile (not . null . trim)
            (dropWhile (null . trim) (drop (flatHeader + 1) content))
      putStrLn ("\ntop " <> show topFlat <> " flat cost centres (individual %time, %alloc):")
      mapM_ (putStrLn . ("  " <>)) (take topFlat flatSection)
      let share = solverShare (drop (treeHeader + 1) content)
      putStrLn ("\ntope solver share (inherited %time of outermost solver rows): "
        <> show share <> "%")
    _ -> do
      hPutStrLn stderr "unrecognised .prof layout"
      exitFailure

solverNames :: [String]
solverNames =
  [ "entailM", "entail", "solveRHSM", "solveRHS", "saturateTopes"
  , "saturateBottom", "saturateInv", "saturateWith", "allTopePoints" ]

isSolverCentre :: String -> Bool
isSolverCentre name =
  base `elem` solverNames || "generateTopes" `isPrefixOf` base
  where base = takeWhile (/= '.') name

-- | Walk the indented call tree, keeping a stack of (depth, under a solver
-- row); sum the inherited %time (second-to-last column) of solver rows with
-- no solver ancestor.
solverShare :: [String] -> Double
solverShare = go [] 0
  where
    go _ acc [] = fromIntegral (round (acc * 10) :: Integer) / 10
    go stack acc (line : rest)
      | null (trim line) = go stack acc rest
      | otherwise =
          let depth = length (takeWhile (== ' ') line)

              cols = words line
              name = concat (take 1 cols)
              stack' = dropWhile (\(d, _) -> d >= depth) stack
              underSolver = any snd stack'
              isSolver = isSolverCentre name
              acc' = case reverse cols of
                (_ : inherited : _)
                  | isSolver && not underSolver
                  , [(t, "")] <- reads inherited -> acc + t
                _ -> acc
          in go ((depth, isSolver || underSolver) : stack') acc' rest

-- * Main

main :: IO ()
main = do
  (cmd, opts) <- getArgs >>= parseArgs
  root <- repoRoot
  let benchDir = root </> "bench"
  case cmd of
    Setup -> do
      corpus <- lookupCorpus (optCorpus opts)
      dir <- ensureCorpus benchDir (optCorpus opts) corpus
      putStrLn ("corpus " <> optCorpus opts <> " ready at " <> dir
        <> " (" <> corpusCommit corpus <> ")")
    Run          -> benchRun root benchDir opts
    Profile      -> benchProfile root benchDir opts
    Summary file -> profSummary file
