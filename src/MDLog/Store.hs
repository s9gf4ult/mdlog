module MDLog.Store where

import Data.Time
import Data.Yaml
import System.Directory
import System.FilePath

-- | Stores document in yaml file
storeDoc
  :: (ToJSON doc)
  => FilePath
  -- ^ Root directory to store doc in
  -> LocalTime
  -- ^ Time used as key to store doc @year/month/day/daytime.json@
  -> doc
  -> IO ()
storeDoc rootDir time doc = do
  filePath <- getNewTimePath rootDir time
  encodeFile filePath doc

getNewTimePath :: FilePath -> LocalTime -> IO FilePath
getNewTimePath rootDir time = go 0
  where
    go :: Int -> IO FilePath
    go suf = do
      let
        fName = timeStr <.> prefix0 suf <.> "yaml"
        dName = joinPath [rootDir, yearStr, monthStr, dayStr]
        fullPath = dName </> fName
      fe <- doesFileExist fullPath
      case fe of
        True -> go $ suf + 1 -- File exists, try other name
        False -> do
          createDirectoryIfMissing True dName
          return fullPath

    timeStr = formatTime defaultTimeLocale "%T" $ localTimeOfDay time
    (year, month, day) = toGregorian $ localDay time
    yearStr = show year
    monthStr = prefix0 month
    dayStr = prefix0 day
    prefix0 a = if a >= 10 then show a else "0" ++ show a
