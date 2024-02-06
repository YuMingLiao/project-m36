{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
import ProjectM36.Server
import ProjectM36.Server.ParseArgs
import Graph.Trace
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  serverConfig <- parseConfig
  ret <- launchServer serverConfig Nothing
  if ret then exitSuccess else exitFailure