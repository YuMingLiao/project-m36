{-# LANGUAGE DerivingVia, DeriveGeneric, DeriveAnyClass, TypeApplications #-}
-- create a simple, on-disk database of ~20 MB so that we can get a heap profile
import ProjectM36.Client
import Data.Text (Text)
import Codec.Winery
import Options.Applicative
import Data.Time.Clock
import GHC.Generics
import Control.DeepSeq
import ProjectM36.Tupleable
import Data.Time.Calendar
import Data.Proxy

data WeatherReading =
  WeatherReading
  { stamp :: UTCTime,
    temperature :: Integer,
    raining :: Bool,
    city :: Text,
    latitude :: Integer,
    longitude :: Integer
    }
   deriving (Generic, Show, Eq, NFData, Tupleable)
   deriving Serialise via WineryRecord WeatherReading

data Opts = Opts { datadir :: FilePath,
                   writeData :: Bool --read or write mode
                 }

parseOptions :: Parser Opts
parseOptions = Opts <$> strOption (long "datadir" <> short 'd') <*> switch (long "write-data" <> short 'w')

main :: IO ()
main = do
  let parser = info (parseOptions <**> helper) (fullDesc <> progDesc "Read or write data for heap profiling.")
  opts <- execParser parser
  let connInfo = InProcessConnectionInfo (MinimalPersistence (datadir opts)) emptyNotificationCallback []
      eCheck v = do
        x <- v
        case x of 
          Left err -> error (show err)
          Right x' -> pure x'
  conn <- eCheck $ connectProjectM36 connInfo
  sessionId <- eCheck $ createSessionAtHead conn "master"
  if writeData opts then do
    putStrLn "writing"
    let addData = map (\i ->
                         WeatherReading { stamp = UTCTime { utctDay = fromGregorian 2022 2 22,
                                                            utctDayTime = secondsToDiffTime i },
                                          temperature = i,
                                          raining = even i,
                                          city = "Mexico City",
                                          latitude = i,
                                          longitude = -i
                                        }) [1..500000]
        defineExpr = toDefineExpr (Proxy @WeatherReading) "x"
    insertExpr <- eCheck (pure $ toInsertExpr addData "x")
    eCheck $ executeDatabaseContextExpr sessionId conn defineExpr    
    eCheck $ executeDatabaseContextExpr sessionId conn insertExpr
    eCheck $ commit sessionId conn
    else do
    putStrLn "reading"
    --read one row to see how heap is affected (will load all rows)
    let readOneRow = Restrict (AttributeEqualityPredicate "temperature" (NakedAtomExpr (IntegerAtom 900))) (RelationVariable "x" ())
    val <- eCheck $ executeRelationalExpr sessionId conn readOneRow
    print val
    
  

