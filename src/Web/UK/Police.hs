{-|
Module      : Web.UK.Police
Description : Bindings to the UK Home Office's Police API
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports types and functions used to implement bindings to the UK
Home Office's Police API.

The following is an example application using the API.

> {-# LANGUAGE LambdaCase #-}
>
> module Main where
>
> import Data.Either (rights)
> import System.Environment (getArgs)
>
> import Data.Text (pack)
> import Data.Text.Read (double)
> import Network.HTTP.Client (newManager)
> import Network.HTTP.Client.TLS (tlsManagerSettings)
>
> import Web.UK.Police
>
> main :: IO ()
> main = do
>   args <- getArgs
>   let ll = CrimeLatLng $ coord args
>   mgr <- newManager tlsManagerSettings
>   crimesStreetDates mgr >>= \case
>     Right result -> do
>       let ms = availableMonths result
>       clrs <- rights <$> mapM (crimesAtLocation mgr ll . Just) ms
>       let css = concatMap (\(CrimesAtLocationResponse cs) -> cs) clrs
>           crimes = map (\(CrimesStreet crime _) -> crime) css
>           ccs = map (\c -> ( cMonth c
>                            , cCategory c
>                            , streetName . slStreet <$> cLocation c)) crimes
>       mapM_ print ccs
>     _ -> pure ()
>  where
>   coord args = let coord' n = either undefined fst $ double $ pack $ args!!n
>                in  LatLng (coord' 0) (coord' 1)
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.UK.Police
  (
  -- * API functions
  -- ** Availability-related
    crimesStreetDates
  , crimeLastUpdated
  -- ** Force-related
  , forces
  , force
  , seniorOfficers
  -- ** Neighbourhood-related
  , neighbourhoods
  , neighbourhood
  , boundary
  , people
  , events
  , priorities
  , locateNeighbourhood
  -- ** Crime reports-related
  , crimesCategories
  , crimesStreet
  , crimesAtLocation
  , crimesNoLocation
  , outcomesStreet
  , outcomesAtLocation
  , outcomesForCrime
  -- ** Stop and search-related
  , stopsStreet
  , stopsAtLocation
  , stopsNoLocation
  , stopsForce
  -- * Utilities
  , availableMonths
  , availableStopAndSearch
  , tForce
  -- * API
  , UKPoliceAPI
  , api
  -- * End point responses
  -- ** Availability-related
  , DataAvailabilityResponse (..)
  , CrimeLastUpdatedResponse (..)
  -- ** Force-related
  , ForcesResponse (..)
  , ForceResponse (..)
  , SeniorOfficersResponse (..)
  -- ** Neighbourhood-related
  , NeighbourhoodsResponse (..)
  , NeighbourhoodResponse (..)
  , BoundaryResponse (..)
  , PeopleResponse (..)
  , EventsResponse (..)
  , PrioritiesResponse (..)
  , LocateNeighbourhoodResponse (..)
  -- ** Crime reports-related
  , CrimesCategoriesResponse (..)
  , CrimesStreetResponse (..)
  , CrimesAtLocationResponse (..)
  , CrimesNoLocationResponse (..)
  , OutcomesAtLocationResponse (..)
  , OutcomesForCrimeResponse (..)
  -- ** Stop and search-related
  , StopsStreetResponse (..)
  , StopsAtLocationResponse (..)
  , StopsNoLocationResponse (..)
  , StopsForceResponse (..)
  -- * Other types
  -- ** Time and date-related
  , YearMonth (..)
  -- ** Georgraphy-related
  , CrimeArea (..)
  , CrimeLocation (..)
  , LatLng (..)
  , Poly (..)
  , LocationId (..)
  , LocationType (..)
  , Street (..)
  , StreetLocation (..)
  , ForceLocation (..)
  -- ** Force and neighbourhood-related
  , Force (..)
  , TForce (..)
  , EWTForce (..)
  , ForceId (..)
  , ForceDescription (..)
  , Neighbourhood (..)
  , NeighbourhoodDescription (..)
  , NeighbourhoodId (..)
  , Priority (..)
  , Person (..)
  -- ** Contact-related
  , ContactDetails (..)
  , EngagementMethod (..)
  , Link (..)
  , Event (..)
  -- ** Crime report-related
  , DataAvailability (..)
  , CrimesCategory (..)
  , CrimesCategoryDescription (..)
  , Crime (..)
  , PersistentId (..)
  , CrimeLastUpdated (..)
  , CrimesStreet (..)
  , Outcome (..)
  , OutcomeAtLocation (..)
  -- ** Stop and search-related
  , Stop (..)
  , StopType (..)
  , Gender (..)
  , StopAgeRange (..)
  , StopOutcome (..)
  , StopOutcomeId (..)
  ) where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv (ClientEnv), ClientError,
  ClientM, Scheme (Https), client, runClientM)

import Data.Time.Calendar.YearMonth
import Web.UK.Police.Types

-- | The api.
api :: Proxy UKPoliceAPI
api = Proxy

crimesStreetDates' :: ClientM DataAvailabilityResponse
forces' :: ClientM ForcesResponse
force'
  :: TForce
  -> ClientM ForceResponse
seniorOfficers'
  :: TForce
  -> ClientM SeniorOfficersResponse
crimesStreet'
  :: CrimesCategory
  -> Maybe Double  -- ^ Latitude
  -> Maybe Double  -- ^ Longitude
  -> Maybe Poly  -- ^ The boundary of the custom area
  -> Maybe YearMonth  -- ^ Date of the crime
  -> ClientM CrimesStreetResponse
outcomesAtLocation'
  :: Maybe LocationId  -- ^ Location id
  -> Maybe Double  -- ^ Latitude
  -> Maybe Double  -- ^ Longitude
  -> Maybe Poly  -- ^ The boundary of the custom area
  -> Maybe YearMonth  -- ^ Date of the outcome
  -> ClientM OutcomesAtLocationResponse
crimesAtLocation'
  :: Maybe LocationId  -- ^ Location id
  -> Maybe Double  -- ^ Latitude
  -> Maybe Double  -- ^ Longitude
  -> Maybe YearMonth  -- ^ Date of the outcome
  -> ClientM CrimesAtLocationResponse
crimesNoLocation'
  :: Maybe CrimesCategory  -- ^ Crimes category
  -> Maybe Force  -- ^ Police force
  -> Maybe YearMonth  -- ^ Date of the outcome
  -> ClientM CrimesNoLocationResponse
crimesCategories'
  :: Maybe YearMonth  -- ^ Month of the data set
  -> ClientM CrimesCategoriesResponse
crimeLastUpdated' :: ClientM CrimeLastUpdatedResponse
outcomesForCrime'
  :: PersistentId  -- ^ Persistent id
  -> ClientM OutcomesForCrimeResponse
neighbourhoods'
  :: TForce  -- ^ Territorial police force
  -> ClientM NeighbourhoodsResponse
neighbourhood'
  :: TForce  -- ^ Territorial police force
  -> NeighbourhoodId
  -> ClientM NeighbourhoodResponse
boundary'
  :: TForce  -- ^ Territorial police force
  -> NeighbourhoodId
  -> ClientM BoundaryResponse
people'
  :: TForce  -- ^ Territorial police force
  -> NeighbourhoodId
  -> ClientM PeopleResponse
events'
  :: TForce  -- ^ Territorial police force
  -> NeighbourhoodId
  -> ClientM EventsResponse
priorities'
  :: TForce  -- ^ Territorial police force
  -> NeighbourhoodId
  -> ClientM PrioritiesResponse
locateNeighbourhood'
  :: Maybe LatLng
  -> ClientM LocateNeighbourhoodResponse
stopsStreet'
  :: Maybe Double  -- ^ Latitude
  -> Maybe Double  -- ^ Longitude
  -> Maybe Poly  -- ^ The boundary of the custom area
  -> Maybe YearMonth  -- ^ Date of the stop and search
  -> ClientM StopsStreetResponse
stopsAtLocation'
  :: Maybe LocationId  -- ^ Location id
  -> Maybe YearMonth  -- ^ Date of the stop and search
  -> ClientM StopsAtLocationResponse
stopsNoLocation'
  :: Maybe Force  -- ^ Police force
  -> Maybe YearMonth  -- ^ Date of the stop and search
  -> ClientM StopsNoLocationResponse
stopsForce'
  :: Maybe Force  -- ^ Police force
  -> Maybe YearMonth  -- ^ Date of the stop and search
  -> ClientM StopsForceResponse

crimesStreetDates' :<|> forces' :<|> force' :<|> seniorOfficers' :<|>
  crimesStreet' :<|> outcomesAtLocation' :<|> crimesAtLocation' :<|>
    crimesNoLocation' :<|> crimesCategories' :<|> crimeLastUpdated' :<|>
      outcomesForCrime' :<|> neighbourhoods' :<|> neighbourhood' :<|>
        boundary' :<|> people' :<|> events' :<|> priorities' :<|>
          locateNeighbourhood' :<|> stopsStreet' :<|> stopsAtLocation' :<|>
            stopsNoLocation' :<|> stopsForce' = client api

policeUKApis :: BaseUrl
policeUKApis = BaseUrl Https "data.police.uk" 443 "/api"

{- | Returns a list of available data sets.
-}
crimesStreetDates
  :: Manager
  -> IO (Either ClientError DataAvailabilityResponse)
crimesStreetDates mgr = runClientM crimesStreetDates'
                                   (ClientEnv mgr policeUKApis Nothing)

{- | A list of all the police forces available via the API.
-}
forces
  :: Manager
  -> IO (Either ClientError ForcesResponse)
forces mgr = runClientM forces' (ClientEnv mgr policeUKApis Nothing)

{- | Returns information about a specific police force.
-}
force
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> IO (Either ClientError ForceResponse)
force mgr fId = runClientM (force' fId) (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of the senior officers for a given police force.
-}
seniorOfficers
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> IO (Either ClientError SeniorOfficersResponse)
seniorOfficers mgr fId = runClientM (seniorOfficers' fId)
                                    (ClientEnv mgr policeUKApis Nothing)

{- | Crimes at street-level; either within a 1 mile radius of a single point, or
within a custom area.
-}
crimesStreet
  :: Manager
  -> CrimesCategory
  -> CrimeArea
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError CrimesStreetResponse)
crimesStreet mgr cc area mYearMonth
  | CrimeCentre (LatLng latitude longitude) <- area
    = runClientM
        (crimesStreet' cc (Just latitude) (Just longitude) Nothing
           mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)
  | CrimePoly poly <- area
    = runClientM
        (crimesStreet' cc Nothing Nothing (Just poly) mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)

{- | Outcomes at street-level, at a specific location.
-}
outcomesStreet
  :: Manager
  -> LocationId
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError OutcomesAtLocationResponse)
outcomesStreet mgr l mYearMonth
  = runClientM (outcomesAtLocation' (Just l) Nothing Nothing Nothing
                  mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | Outcomes at street-level; either within a 1 mile radius of a single point,
or within a custom area.
-}
outcomesAtLocation
  :: Manager
  -> CrimeArea
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError OutcomesAtLocationResponse)
outcomesAtLocation mgr area mYearMonth
  | CrimeCentre (LatLng latitude longitude) <- area
    = runClientM
        (outcomesAtLocation' Nothing (Just latitude) (Just longitude) Nothing
           mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)
  | CrimePoly poly <- area
    = runClientM
        (outcomesAtLocation' Nothing Nothing Nothing (Just poly) mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)

{- | Returns just the crimes which occurred at the specified location, rather
than those within a radius. If given latitude and longitude, finds the nearest
pre-defined location and returns the crimes which occurred there.
-}
crimesAtLocation
  :: Manager
  -> CrimeLocation
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError CrimesAtLocationResponse)
crimesAtLocation mgr loc mYearMonth
  | CrimeLatLng (LatLng latitude longitude) <- loc
    = runClientM
        (crimesAtLocation' Nothing (Just latitude) (Just longitude)
           mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)
  | CrimeLocation locationId <- loc
    = runClientM
        (crimesAtLocation' (Just locationId) Nothing Nothing mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of crimes that could not be mapped to a location.
-}
crimesNoLocation
  :: Manager
  -> CrimesCategory
  -> Force
  -> Maybe YearMonth
  -> IO (Either ClientError CrimesNoLocationResponse)
crimesNoLocation mgr cc f mYearMonth
  = runClientM (crimesNoLocation' (Just cc) (Just f) mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of valid categories for a given data set date.
-}
crimesCategories
  :: Manager
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError CrimesCategoriesResponse)
crimesCategories mgr mYearMonth
  = runClientM (crimesCategories' mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | When the crime data was last updated.
-}
crimeLastUpdated
  :: Manager
  -> IO (Either ClientError CrimeLastUpdatedResponse)
crimeLastUpdated mgr
  = runClientM crimeLastUpdated' (ClientEnv mgr policeUKApis Nothing)

{- | Returns the outcomes (case history) for the specified crime.
-}
outcomesForCrime
  :: Manager
  -> PersistentId
  -> IO (Either ClientError OutcomesForCrimeResponse)
outcomesForCrime mgr pId
  = runClientM (outcomesForCrime' pId) (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of valid neighbourhood identifiers and names for a given
police force.
-}
neighbourhoods
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> IO (Either ClientError NeighbourhoodsResponse)
neighbourhoods mgr f = runClientM (neighbourhoods' f)
                                  (ClientEnv mgr policeUKApis Nothing)

{- | Returns a description of a given neighbourhood.
-}
neighbourhood
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> NeighbourhoodId
  -> IO (Either ClientError NeighbourhoodResponse)
neighbourhood mgr f n = runClientM (neighbourhood' f n)
                                   (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of latitude/longitude pairs that make up the boundary of a
given neigbourhood.
-}
boundary
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> NeighbourhoodId
  -> IO (Either ClientError BoundaryResponse)
boundary mgr f n = runClientM (boundary' f n)
                              (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of members of team for a given neighbourhood.
-}
people
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> NeighbourhoodId
  -> IO (Either ClientError PeopleResponse)
people mgr f n = runClientM (people' f n) (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of future events for a given neighbourhood.
-}
events
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> NeighbourhoodId
  -> IO (Either ClientError EventsResponse)
events mgr f n = runClientM (events' f n) (ClientEnv mgr policeUKApis Nothing)

{- | Returns a list of priorities for a given neighbourhood.
-}
priorities
  :: Manager
  -> TForce  -- ^ Territorial police force.
  -> NeighbourhoodId
  -> IO (Either ClientError PrioritiesResponse)
priorities mgr f n = runClientM (priorities' f n)
                                (ClientEnv mgr policeUKApis Nothing)

{- | Find the neighbourhood policing team responsible for a particular area.
-}
locateNeighbourhood
  :: Manager
  -> LatLng
  -> IO (Either ClientError LocateNeighbourhoodResponse)
locateNeighbourhood mgr ll = runClientM (locateNeighbourhood' (Just ll))
                                        (ClientEnv mgr policeUKApis Nothing)

{- | Stop and searches at street-level; either within a 1 mile radius of a
single point, or within a custom area.
-}
stopsStreet
  :: Manager
  -> CrimeArea
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError StopsStreetResponse)
stopsStreet mgr area mYearMonth
  | CrimeCentre (LatLng latitude longitude) <- area
    = runClientM
        (stopsStreet' (Just latitude) (Just longitude) Nothing mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)
  | CrimePoly poly <- area
    = runClientM
        (stopsStreet' Nothing Nothing (Just poly) mYearMonth)
        (ClientEnv mgr policeUKApis Nothing)

{- | Stop and searches at a particular location.
-}
stopsAtLocation
  :: Manager
  -> LocationId
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError StopsAtLocationResponse)
stopsAtLocation mgr locationId mYearMonth
  = runClientM (stopsAtLocation' (Just locationId) mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | Stop and searches that could not be mapped to a location.
-}
stopsNoLocation
  :: Manager
  -> Force  -- ^ Police force.
  -> Maybe YearMonth  -- ^ The lastest month will be used by default.
  -> IO (Either ClientError StopsNoLocationResponse)
stopsNoLocation mgr f mYearMonth
  = runClientM (stopsNoLocation' (Just f) mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | Stop and searches reported by a particular force.
-}
stopsForce
  :: Manager
  -> Force  -- ^ Police force.
  -> Maybe YearMonth  -- ^ The latest month will be used by default.
  -> IO (Either ClientError StopsForceResponse)
stopsForce mgr f mYearMonth
  = runClientM (stopsForce' (Just f) mYearMonth)
               (ClientEnv mgr policeUKApis Nothing)

{- | Returns a sorted list of months for which data is available, given a
'DataAvailabilityResponse'.
-}
availableMonths :: DataAvailabilityResponse -> [YearMonth]
availableMonths (DataAvailabilityResponse das) = sort $ map dataDate das

{- | Returns a sorted list of months for which stop and search data is
available, given a 'DataAvailabilityResponse' and a participating police force.
-}
availableStopAndSearch :: DataAvailabilityResponse -> Force -> [YearMonth]
availableStopAndSearch (DataAvailabilityResponse das) f
  = sort $ mapMaybe available das
 where
  available da = if f `elem` dataStopAndSearch da
    then Just $ dataDate da
    else Nothing

{- | Attempts to cast a police force to a territorial police force.
-}
tForce :: Force -> Maybe TForce
tForce (TF f) = Just f
tForce _      = Nothing
