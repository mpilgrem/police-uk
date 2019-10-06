{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad (forM_)
import Data.Maybe (fromJust, mapMaybe)
import Data.Vector.Unboxed.Sized (fromList)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client (ClientError)
import System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute),
  OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)

import Data.Time.Calendar.YearMonth
import Web.UK.Police

data Flag
  = FlagCrimesStreetDates
  | FlagForces
  | FlagForce
  | FlagSeniorOfficers
  | FlagCrimesStreet
  | FlagOutcomesStreet
  | FlagOutcomesAtLocation
  | FlagCrimesAtLocation
  | FlagCrimesNoLocation
  | FlagCrimesCategories
  | FlagCrimeLastUpdated
  | FlagOutcomesForCrime
  | FlagNeighbourhoods
  | FlagNeighbourhood
  | FlagBoundary
  | FlagPeople
  | FlagEvents
  | FlagPriorities
  | FlagLocateNeighbourhood
  | FlagStopsStreet
  | FlagStopsAtLocation
  | FlagStopsNoLocation
  | FlagStopsForce
  | Help
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option [] ["crimes-street-dates"] (NoArg FlagCrimesStreetDates) "Availability"
  , Option [] ["forces"] (NoArg FlagForces) "List of forces"
  , Option [] ["force"] (NoArg FlagForce) "Specific force"
  , Option [] ["senior-officers"] (NoArg FlagSeniorOfficers) "Senior officers"
  , Option [] ["crimes-street"] (NoArg FlagCrimesStreet) "Street-level crimes"
  , Option [] ["outcomes-street"] (NoArg FlagOutcomesStreet) "Street-level outcomes (area)"
  , Option [] ["outcomes-at-location"] (NoArg FlagOutcomesAtLocation) "Street-level outcomes (location)"
  , Option [] ["crimes-at-location"] (NoArg FlagCrimesAtLocation) "Crimes at location"
  , Option [] ["crimes-no-location"] (NoArg FlagCrimesNoLocation) "Crimes with no location"
  , Option [] ["crimes-categories"] (NoArg FlagCrimesCategories) "Crime categories"
  , Option [] ["crime-last-updated"] (NoArg FlagCrimeLastUpdated) "Last updated"
  , Option [] ["outcomes-for-crime"] (NoArg FlagOutcomesForCrime) "Outcomes for a specific crime"
  , Option [] ["neighbourhoods"] (NoArg FlagNeighbourhoods) "List of neighbourhoods for a force"
  , Option [] ["neighbourhood"] (NoArg FlagNeighbourhood) "Specific neighbourhood"
  , Option [] ["boundary"] (NoArg FlagBoundary) "Neighbourhood boundary"
  , Option [] ["people"] (NoArg FlagPeople) "Neighbourhood team"
  , Option [] ["events"] (NoArg FlagEvents) "Neighbourhood events"
  , Option [] ["priorities"] (NoArg FlagPriorities) "Neighbourhood priorities"
  , Option [] ["locate-neighbourhood"] (NoArg FlagLocateNeighbourhood) "Locate a neighbourhood"
  , Option [] ["stops-street"] (NoArg FlagStopsStreet) "Stop and searches by area"
  , Option [] ["stops-at-location"] (NoArg FlagStopsAtLocation) "Stop and searches by location"
  , Option [] ["stops-no-location"] (NoArg FlagStopsNoLocation) "Stop and searches with no location"
  , Option [] ["stops-force"] (NoArg FlagStopsForce) "Stop and searches by force"
  , Option ['h'] ["help"]  (NoArg Help) "Show this help text"
  ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts args =
  case getOpt Permute options args of
    (o, n, []) -> pure (o, n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usage))

usage :: String
usage = usageInfo header options
 where
  header = "Usage: police-uk [OPTIONS]"

main :: IO ()
main = do
  args <- getArgs
  (o, _) <- compilerOpts args
  if null o || elem Help o
    then putStrLn usage
    else do
      putStrLn "A test of the UK Home Office's Police API."
      putStrLn "------------------------------------------"
      mgr <- newManager tlsManagerSettings
      mapM_ (main' mgr) o

main' :: Manager -> Flag -> IO ()
main' mgr flag
  | flag == FlagCrimesStreetDates = crimesStreetDatesEx
  | flag == FlagForces = forcesEx
  | flag == FlagForce = forceEx
  | flag == FlagSeniorOfficers = seniorOfficersEx
  | flag == FlagCrimesStreet = crimesStreetEx
  | flag == FlagOutcomesStreet = outcomesStreetEx
  | flag == FlagOutcomesAtLocation = outcomesAtLocationEx
  | flag == FlagCrimesAtLocation = crimesAtLocationEx
  | flag == FlagCrimesNoLocation = crimesNoLocationEx
  | flag == FlagCrimesCategories = crimesCategoriesEx
  | flag == FlagCrimeLastUpdated = crimeLastUpdatedEx
  | flag == FlagOutcomesForCrime = outcomesForCrimeEx
  | flag == FlagNeighbourhoods = neighbourhoodsEx
  | flag == FlagNeighbourhood = neighbourhoodEx
  | flag == FlagBoundary = boundaryEx
  | flag == FlagPeople = peopleEx
  | flag == FlagEvents = eventsEx
  | flag == FlagPriorities = prioritiesEx
  | flag == FlagLocateNeighbourhood = locateNeighbourhoodEx
  | flag == FlagStopsStreet = stopsStreetEx
  | flag == FlagStopsAtLocation = stopsAtLocationEx
  | flag == FlagStopsNoLocation = stopsNoLocationEx
  | flag == FlagStopsForce = stopsForceEx
  | otherwise = putStrLn "No example is implemented."
 where
  crimesStreetDatesEx = processResponse =<< crimesStreetDates mgr

  forcesEx = processResponse =<< forces mgr

  forceEx = do
    forceEx' $ EW Leicestershire
    putStrLn "All"
    r <- forces mgr
    case r of
      Right (ForcesResponse result) -> do
        let result' = mapMaybe (tForce . forceId) result
        forM_ result' forceEx'
      _ -> putStrLn $ "Error! Response:\n" ++ show r
   where
    forceEx' f = do
      print f
      processResponse =<< force mgr f

  seniorOfficersEx = do
    seniorOfficersEx' $ EW Leicestershire
    putStrLn "All"
    r <- forces mgr
    case r of
      Right (ForcesResponse result) -> do
        let result' = mapMaybe (tForce . forceId) result
        forM_ result' seniorOfficersEx'
      _ -> putStrLn $ "Error! Response:\n" ++ show r
   where
    seniorOfficersEx' f = do
      print f
      processResponse =<< seniorOfficers mgr f

  crimesStreetEx = do
    let m = Just $ YearMonth (2017, 1)
    forM_ [l1, l2] (\l -> processResponse =<< crimesStreet mgr AllCrime l m)

  outcomesStreetEx = do
    let m = Just $ YearMonth (2017, 1)
    processResponse =<< outcomesStreet mgr l3 m

  outcomesAtLocationEx = do
    let m = Just $ YearMonth (2017, 1)
    forM_ [l1, l2] (\l -> processResponse =<< outcomesAtLocation mgr l m)

  crimesAtLocationEx = do
    let m = Just $ YearMonth (2017, 2)
    forM_ [l4, l5] (\l -> processResponse =<< crimesAtLocation mgr l m)

  crimesNoLocationEx = do
    let m = Just $ YearMonth (2017, 3)
    processResponse =<< crimesNoLocation mgr AllCrime (TF $ EW Leicestershire) m

  crimesCategoriesEx = do
    let m = Just $ YearMonth (2011, 8)
    processResponse =<< crimesCategories mgr m

  crimeLastUpdatedEx = processResponse =<< crimeLastUpdated mgr

  outcomesForCrimeEx = do
    let p = "590d68b69228a9ff95b675bb4af591b38de561aa03129dc09a03ef34f537588c"
        pId = PersistentId $ fromJust $ fromList p
    processResponse =<< outcomesForCrime mgr pId

  neighbourhoodsEx = processResponse =<< neighbourhoods mgr (EW Leicestershire)

  neighbourhoodEx = processResponse =<< neighbourhood mgr (EW Leicestershire) n1

  boundaryEx = processResponse =<< boundary mgr (EW Leicestershire) n1

  peopleEx = processResponse =<< people mgr (EW Leicestershire) n1

  eventsEx = processResponse =<< events mgr (EW Leicestershire) n1

  prioritiesEx = processResponse =<< priorities mgr (EW Leicestershire) n1

  locateNeighbourhoodEx =
    processResponse =<< locateNeighbourhood mgr (LatLng 51.500617 -0.124629)

  stopsStreetEx = do
    let m = Just $ YearMonth (2018, 6)
    forM_ [l1, l6] (\l -> processResponse =<< stopsStreet mgr l m)

  stopsAtLocationEx = do
    let m = Just $ YearMonth (2017, 1)
    processResponse =<< stopsAtLocation mgr (LocationId 883407) m

  stopsNoLocationEx = do
    let m = Just $ YearMonth (2017, 1)
    processResponse =<< stopsNoLocation mgr (TF $ EW Cleveland) m

  stopsForceEx = do
    let m = Just $ YearMonth (2017, 1)
    processResponse =<< stopsForce mgr (TF $ EW AvonAndSomerset) m

processResponse :: Show a => Either ClientError a -> IO ()
processResponse r =
  case r of
    Right result -> print result
    _ -> putStrLn $ "Error! Response:\n" ++ show r

-- Example crime locations and areas
ll :: LatLng
ll = LatLng 52.629729 -1.131592
l1, l2, l6 :: CrimeArea
l1 = CrimeCentre ll
l2 = CrimePoly p
 where
  p = Poly [ LatLng 52.268 0.543
           , LatLng 52.794 0.238
           , LatLng 52.130 0.478
           ]
l6 = CrimePoly p
 where
  p = Poly [ LatLng 52.2 0.5
           , LatLng 52.8 0.2
           , LatLng 52.1 0.88
           ]

l3 :: LocationId
l3 = LocationId 883498

l4, l5 :: CrimeLocation
l4 = CrimeLatLng ll
l5 = CrimeLocation (LocationId 884227)

n1 :: NeighbourhoodId
n1 = NeighbourhoodId "NC04"
