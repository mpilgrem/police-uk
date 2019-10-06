{-|
Module      : Web.UK.Police.Types
Description : UK Home Office's Police API and related types
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports a type used to represent the UK Home Office's Police API
('UKPoliceAPI') and other types used to represent values for querying, and
obtaining information from, that API.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

module Web.UK.Police.Types
  (
  -- * API
    UKPoliceAPI
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

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), Object, Value (..), (.:), (.:?), (<?>),
  camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, liftParseJSON,
  withObject, withText)
import Data.Aeson.Types (JSONPathElement (Key), Parser, listParser)
import Data.Char (toLower)
import qualified Data.HashMap.Strict as H (lookup)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T (intercalate, length)
import Data.Text.Read (double)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (LocalTime, ZonedTime, zonedTimeToUTC)
import Data.Vector.Unboxed.Sized (Vector, foldr', fromListN)
import GHC.Generics (Generic)
import Servant.API ((:<|>), (:>), Capture, Get, JSON, QueryParam,
  ToHttpApiData, toUrlPiece)

import Data.Time.Calendar.YearMonth
import Web.UK.Police.Types.EWTForce
import Web.UK.Police.Types.CrimesCategory
import Web.UK.Police.Types.OutcomeCategory

-- | Type representing the UK Home Office's Police API.
type UKPoliceAPI
  =    "crimes-street-dates"
  :>   Get '[JSON] DataAvailabilityResponse

  :<|> "forces"
  :>   Get '[JSON] ForcesResponse

  :<|> "forces"
  :>   Capture "force" TForce
  :>   Get '[JSON] ForceResponse

  :<|> "forces"
  :>   Capture "force" TForce
  :>   "people"
  :>   Get '[JSON] SeniorOfficersResponse

  :<|> "crimes-street"
  :>   Capture "crime-category" CrimesCategory
  :>   QueryParam "lat" Double
  :>   QueryParam "lng" Double
  :>   QueryParam "poly" Poly
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] CrimesStreetResponse

  :<|> "outcomes-at-location"
  :>   QueryParam "location_id" LocationId
  :>   QueryParam "lat" Double
  :>   QueryParam "lng" Double
  :>   QueryParam "poly" Poly
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] OutcomesAtLocationResponse

  :<|> "crimes-at-location"
  :>   QueryParam "location_id" LocationId
  :>   QueryParam "lat" Double
  :>   QueryParam "lng" Double
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] CrimesAtLocationResponse

  :<|> "crimes-no-location"
  :>   QueryParam "category" CrimesCategory
  :>   QueryParam "force" Force
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] CrimesNoLocationResponse

  :<|> "crime-categories"
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] CrimesCategoriesResponse

  :<|> "crime-last-updated"
  :>   Get '[JSON] CrimeLastUpdatedResponse

  :<|> "outcomes-for-crime"
  :>   Capture "peristent_id" PersistentId
  :>   Get '[JSON] OutcomesForCrimeResponse

  :<|> Capture "force" TForce
  :>   "neighbourhoods"
  :>   Get '[JSON] NeighbourhoodsResponse

  :<|> Capture "force" TForce
  :>   Capture "neighbourhood_id" NeighbourhoodId
  :>   Get '[JSON] NeighbourhoodResponse

  :<|> Capture "force" TForce
  :>   Capture "neighbourhood_id" NeighbourhoodId
  :>   "boundary"
  :>   Get '[JSON] BoundaryResponse

  :<|> Capture "force" TForce
  :>   Capture "neighbourhood_id" NeighbourhoodId
  :>   "people"
  :>   Get '[JSON] PeopleResponse

  :<|> Capture "force" TForce
  :>   Capture "neighbourhood_id" NeighbourhoodId
  :>   "events"
  :>   Get '[JSON] EventsResponse

  :<|> Capture "force" TForce
  :>   Capture "neighbourhood_id" NeighbourhoodId
  :>   "priorities"
  :>   Get '[JSON] PrioritiesResponse

  :<|> "locate-neighbourhood"
  :>   QueryParam "q" LatLng
  :>   Get '[JSON] LocateNeighbourhoodResponse

  :<|> "stops-street"
  :>   QueryParam "lat" Double
  :>   QueryParam "lng" Double
  :>   QueryParam "poly" Poly
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] StopsStreetResponse

  :<|> "stops-at-location"
  :>   QueryParam "location_id" LocationId
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] StopsAtLocationResponse

  :<|> "stops-no-location"
  :>   QueryParam "force" Force
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] StopsNoLocationResponse

  :<|> "stops-force"
  :>   QueryParam "force" Force
  :>   QueryParam "date" YearMonth
  :>   Get '[JSON] StopsForceResponse

-- | Type representing the response to an end point of the API.
newtype DataAvailabilityResponse
  = DataAvailabilityResponse [DataAvailability]
  deriving (Eq, FromJSON, Show)

{- | Type representing months for which reported crime is available and a list
of police forces providing stop and search information for the month in
question.
-}
data DataAvailability = DataAvailability
  { dataDate          :: YearMonth
  , dataStopAndSearch :: [Force]
  } deriving (Eq, Generic, Show)

instance FromJSON DataAvailability where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '-' . drop 4 }

-- | Type representing participating UK police forces
data Force
  = TF TForce  -- ^ Territorial police force
  | BTP        -- ^ British Transport Police
  deriving (Eq)

instance Show Force where
  show (TF f) = show f
  show BTP = "British Transport Police"

instance FromJSON Force where
  parseJSON v = parseTForce <|> parseBTP v
   where
    parseTForce = TF <$> parseJSON v
    parseBTP = withText "BTP" $ \case
      "btp" -> pure BTP
      _ -> fail "Not a valid UK police force"

instance ToHttpApiData Force where
  toUrlPiece (TF f) = toUrlPiece f
  toUrlPiece BTP = "btp"

-- | Type representing participating UK territorial police forces
data TForce
  = EW EWTForce  -- ^ Territorial police force in England and Wales
  | NorthernIreland  -- ^ Police Service of Northern Ireland
  deriving (Eq)

instance Show TForce where
  show (EW f) = show f
  show NorthernIreland = "Police Service of Northern Ireland"

instance FromJSON TForce where
  parseJSON v = parseEWTForce <|> parseNI v
   where
    parseEWTForce = EW <$> parseJSON v
    parseNI = withText "NorthernIreland" $ \case
      "northern-ireland" -> pure NorthernIreland
      _ -> fail "Not a valid territorial police force"

instance ToHttpApiData TForce where
  toUrlPiece (EW f) = toUrlPiece f
  toUrlPiece NorthernIreland = "northern-ireland"

-- | Type representing the response to an end point of the API.
newtype ForcesResponse = ForcesResponse [ForceId]
  deriving (Eq, FromJSON, Show)

-- | Type representing identifiers and associated names of police forces.
data ForceId = ForceId
  { forceId   :: Force
  , forceName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ForceId where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '-' . drop 5 }

-- | Type representing the response to an end point of the API.
newtype ForceResponse = ForceResponse ForceDescription
  deriving (Eq, FromJSON, Show)

-- | Type representing descriptions of police forces.
data ForceDescription = ForceDescription
  { fdId                :: Force
  , fdName              :: Text
  , fdDescription       :: Maybe Text
  , fdUrl               :: Text
  , fdTelephone         :: Text
  , fdEngagementMethods :: Maybe [EngagementMethod]
  } deriving (Eq, Generic, Show)

instance FromJSON ForceDescription where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '-' . drop 2 }

-- | Type representing engagement methods.
data EngagementMethod = EngagementMethod
  { emUrl         :: Text
  , emDescription :: Text
  , emTitle       :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON EngagementMethod where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '-' . drop 2 }

-- | Type representing the response to an end point of the API.
newtype SeniorOfficersResponse = SeniorOfficersResponse [Person]
  deriving (Eq, FromJSON, Show)

-- | Type representing contact details.
data ContactDetails = ContactDetails
  { cdEmail      :: Maybe Text
  , cdTelephone  :: Maybe Text
  , cdMobile     :: Maybe Text
  , cdFax        :: Maybe Text
  , cdWeb        :: Maybe Text
  , cdAddress    :: Maybe Text
  , cdFacebook   :: Maybe Text
  , cdTwitter    :: Maybe Text
  , cdYoutube    :: Maybe Text
  , cdMyspace    :: Maybe Text
  , cdBebo       :: Maybe Text
  , cdFlickr     :: Maybe Text
  , cdGooglePlus :: Maybe Text
  , cdForum      :: Maybe Text
  , cdEMessaging :: Maybe Text
  , cdBlog       :: Maybe Text
  , cdRss        :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON ContactDetails where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '-' . drop 2 }

-- | Type representing the response to an end point of the API.
newtype CrimesCategoriesResponse
  = CrimesCategoriesResponse [CrimesCategoryDescription]
  deriving (Eq, FromJSON, Show)

{- | Type representing identifiers and associated descriptions of crimes
categories.
-}
data CrimesCategoryDescription = CrimesCategoryDescription
  { ccUrl  :: CrimesCategory
  , ccName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON CrimesCategoryDescription where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = map toLower . drop 2 }

-- The geometry used by the Police API (as at 27 Oct 2019)

--                        LatLng     Poly Location
--                        ------   ------ --------
-- crimes-street               X        X
-- stops-street                X        X
-- outcomes-at-location        X        X
--   "                                           X  <- split, logically
-- crimes-at-location          X                 X  <- special case
-- stops-at-location                             X

-- | Type representing latitude and longitude coordinates
data LatLng = LatLng
  { lat :: Double
  , lng :: Double
  } deriving (Eq, Show)

instance FromJSON LatLng where
  parseJSON = withObject "LatLng" $ \o -> do
    mLatLng <- parseMaybeLatLng o
    case mLatLng of
      Just ll -> pure ll
      _       -> fail "No latitude or no longitude"

parseMaybeLatLng :: Object -> Parser (Maybe LatLng)
parseMaybeLatLng o = do
  mLatText <- o .:? "latitude"
  mLngText <- o .:? "longitude"
  case (mLatText, mLngText) of
    (Just latText, Just lngText) -> case (double latText, double lngText) of
      (Left msg, _) -> fail msg
      (_, Left msg) -> fail msg
      (Right (latitude, _), Right (longitude, _)) ->
        pure $ Just $ LatLng latitude longitude
    _ -> pure Nothing

instance ToHttpApiData LatLng where
  toUrlPiece ll = toUrlPiece (lat ll) <> "," <> toUrlPiece (lng ll)

-- | Type representing boundaries of regions
newtype Poly = Poly [LatLng]
  deriving (Eq, Show)

instance ToHttpApiData Poly where
  toUrlPiece (Poly lls) = T.intercalate ":" $ map toUrlPiece lls

-- | Type representing areas used to obtain crime reports
data CrimeArea
  = CrimeCentre LatLng  -- ^ Centre of a circle of radius 1 mile
  | CrimePoly Poly  -- ^ Custom area
  deriving (Eq, Show)

-- | Type representing location used to obtain crime reports
data CrimeLocation
  = CrimeLocation LocationId
  | CrimeLatLng LatLng
  deriving (Eq, Show)

-- The coding of outcomes by the Police UK API (as at 27 Oct 2019)
--
-- crimes-street:        outcome_status
-- crimes-at-location:     category [text]
-- crimes-no-location:     date
--
-- outcomes-at-location: category
-- outcomes-for-crime:     code
--                         name [text]
--                       date
--                       person_id

-- | Type representing the response to an end point of the API.
newtype CrimesStreetResponse = CrimesStreetResponse [CrimesStreet]
  deriving (Eq, FromJSON, Show)

-- | Type representing reported crimes and their associated outcomes.
data CrimesStreet = CrimesStreet Crime (Maybe Outcome)
  deriving (Eq, Show)

instance FromJSON CrimesStreet where
  parseJSON = withObject "CrimeStreet" $ \o -> do
    cCategory        <- o .: "category"
    cLocationType    <- o .: "location_type"
    cLocationSubtype <- o .: "location_subtype"
    cLocation        <- o .: "location"
    cContext         <- o .: "context"
    cPersistentId    <- o .:?? "persistent_id"
    cId              <- o .: "id"
    cMonth           <- o .: "month"
    outcomeStatus    <- o .:? "outcome_status"
    outcome          <- sequenceA $ parseOutcome' <$> outcomeStatus
    pure $ CrimesStreet Crime {..} outcome

-- | Type representing crime reports.
data Crime = Crime
  { cCategory        :: CrimesCategory
  , cLocationType    :: Maybe LocationType
  , cLocationSubtype :: Maybe Text
  , cLocation        :: Maybe StreetLocation
  , cContext         :: Text
  , cPersistentId    :: Maybe PersistentId
  , cId              :: Integer
  , cMonth           :: YearMonth
  } deriving (Eq, Generic, Show)

instance FromJSON Crime where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

-- | Type representing types of locations.
data LocationType
  = LTForce
  | LTBTP
  | UnknownLocationType Text
  deriving (Eq, Show)

instance FromJSON LocationType where
  parseJSON = withText "LocationType" $ \t -> case t of
      "Force" -> pure LTForce
      "BTP"   -> pure LTBTP
      _       -> pure $ UnknownLocationType t

-- | Type representating streets and their locations
data StreetLocation = StreetLocation
  { slLatLng :: LatLng
  , slStreet :: Street
  } deriving (Eq, Generic, Show)

instance FromJSON StreetLocation where
  parseJSON v = do
    ll <- parseJSON v
    street <- (withObject "StreetLocation" $ \o -> o .: "street") v
    pure $ StreetLocation ll street

-- | Type representing streets
data Street = Street
  { streetId   :: LocationId
  , streetName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Street where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Type representing identifiers for anonymised locations
newtype LocationId = LocationId Int
  deriving (Eq, FromJSON, Show, ToHttpApiData)

-- | Type representing outcomes for crime reports.
data Outcome = Outcome
  { oCategory :: OutcomeCategory
  , oPersonId :: Maybe Int
  , oMonth    :: YearMonth
  } deriving (Eq, Show)

instance FromJSON Outcome where
  parseJSON = withObject "Outcome" parseOutcome

-- | Helper function to parse an Outcome from an object
parseOutcome :: Object -> Parser Outcome
parseOutcome o = do
  category  <- o .: "category"
  oCategory <- category .: "code"
  oPersonId <- o .: "person_id"
  oMonth    <- o .: "date"
  pure $ Outcome {..}

-- | Helper function to parse an Outcome from an object
parseOutcome' :: Object -> Parser Outcome
parseOutcome' o = do
  oCategory  <- o .: "category"
  let oPersonId = Nothing
  oMonth    <- o .: "date"
  pure $ Outcome {..}

-- | Type representing persistent identifiers for UK crime reports.
newtype PersistentId = PersistentId (Vector 64 Char)
  deriving (Eq, Show)

instance FromJSON PersistentId where
  parseJSON = withText "PersistentId" $ \t ->
    if T.length t == 64
      then case fromListN $ unpack t of
          Just v -> pure (PersistentId v)
          _ -> fail "Unknown reason"
      else if t == ""
        then parseJSON Null
        else fail "Not 64 characters in length"

instance ToHttpApiData PersistentId where
  toUrlPiece (PersistentId pId) = pack $ foldr' (:) "" pId

-- | Type representing the response to an end point of the API.
newtype OutcomesAtLocationResponse
  = OutcomesAtLocationResponse [OutcomeAtLocation]
  deriving (Eq, FromJSON, Show)

-- | Type representing outcomes and associated crimes with a location.
data OutcomeAtLocation = OutcomeAtLocation Crime Outcome
  deriving (Eq, Show)

instance FromJSON OutcomeAtLocation where
  parseJSON = withObject "OutcomeAtLocation" $ \o -> do
    crime <- o .: "crime"
    outcome <- parseOutcome o
    pure $ OutcomeAtLocation crime outcome

-- | Type representing the response to an end point of the API.
newtype CrimesAtLocationResponse = CrimesAtLocationResponse [CrimesStreet]
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype CrimesNoLocationResponse = CrimesNoLocationResponse [CrimesStreet]
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype CrimeLastUpdatedResponse = CrimeLastUpdatedResponse CrimeLastUpdated
  deriving (Eq, FromJSON, Show)

-- | Type representing dates on which reported crime data was last updated.
newtype CrimeLastUpdated = CrimeLastUpdated {
  cluDate :: Day
  } deriving (Eq, Generic, Show)

instance FromJSON CrimeLastUpdated where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 3 }

-- | Type representing the response to an end point of the API.
data OutcomesForCrimeResponse = OutcomesForCrimeResponse Crime [Outcome]
  deriving (Eq, Show)

instance FromJSON OutcomesForCrimeResponse where
  parseJSON = withObject "OutcomesForCrimeResponse" $ \o -> do
    c  <- o .: "crime"
    os <- o .: "outcomes"
    pure $ OutcomesForCrimeResponse c os

-- | Type representing the response to an end point of the API.
newtype NeighbourhoodsResponse = NeighbourhoodsResponse [Neighbourhood]
  deriving (Eq, FromJSON, Show)

-- | Type representing identification of neighbourhoods.
data Neighbourhood = Neighbourhood
  { nId   :: NeighbourhoodId
  , nName :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Neighbourhood where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

-- | Type representing identifiers for neighbourhoods.
newtype NeighbourhoodId = NeighbourhoodId Text
  deriving (Eq, FromJSON, Show, ToHttpApiData)

-- | Type representing the response to an end point of the API.
data NeighbourhoodResponse
  = NeighbourhoodResponse Neighbourhood
                          NeighbourhoodDescription
                          (Maybe ContactDetails)
  deriving (Eq, Show)

instance FromJSON NeighbourhoodResponse where
  parseJSON = withObject "NeighbourhoodResponse" $ \o -> do
    nId             <- o .: "id"
    nName           <- o .: "name"
    nUrlForce       <- o .: "url_force"
    nWelcomeMessage <- o .:? "welcome_message"
    nLinks          <- o .: "links"
    nCentre         <- o .: "centre"
    nLocations      <- o .: "locations"
    nPopulation     <- o .: "population"
    nDescription    <- o .: "description"
    nContactDetails <- o .:? "contract_details"
    pure $ NeighbourhoodResponse Neighbourhood {..}
                                 NeighbourhoodDescription {..}
                                 nContactDetails

-- | Type representing descriptions of a neighbourhood.
data NeighbourhoodDescription = NeighbourhoodDescription
  { nUrlForce       :: Text
  , nWelcomeMessage :: Maybe Text
  , nLinks          :: [Link]
  , nCentre         :: LatLng
  , nLocations      :: [ForceLocation]
  , nPopulation     :: Text
  , nDescription    :: Text
  } deriving (Eq, Generic, Show)

-- | Type representing links to information on the internet.
data Link = Link
  { linkUrl         :: Text
  , linkTitle       :: Text
  , linkDescription :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON Link where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 4 }

-- | Type representing locations of police.
data ForceLocation = ForceLocation
  { flName        :: Text
  , flType        :: Text
  , flDescription :: Maybe Text
  , flLocation    :: Maybe LatLng
  , flAddress     :: Text
  , flPostcode    :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON ForceLocation where
  parseJSON = withObject "ForceLocation" $ \o -> do
    flName        <- o .: "name"
    flType        <- o .: "type"
    flDescription <- o .: "description"
    flLocation    <- parseMaybeLatLng o
    flAddress     <- o .: "address"
    flPostcode    <- o .: "postcode"
    pure $ ForceLocation {..}

-- | Type representing the response to an end point of the API.
newtype BoundaryResponse = BoundaryResponse [LatLng]
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype PeopleResponse = PeopleResponse [Person]
  deriving (Eq, FromJSON, Show)

{- | Type representing people, either senior officers or in a neighbourhood
team.
-}
data Person = Person
  { pName           :: Text
  , pRank           :: Maybe Text
  , pBio            :: Maybe Text
  , pContactDetails :: Maybe ContactDetails
  } deriving (Eq, Generic, Show)

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

-- | Type representing the response to an end point of the API.
newtype EventsResponse = EventsResponse [Event]
  deriving (Eq, FromJSON, Show)

-- | Type representing neighbourhood events.
data Event = Event
  { eTitle          :: Text
  , eType           :: Text
  , eDescription    :: Maybe Text
  , eContactDetails :: Maybe ContactDetails
  , eAddress        :: Text
  , eStartDate      :: LocalTime
  , eEndDate        :: LocalTime
  } deriving (Eq, Generic, Show)

instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

-- | Type representing the response to an end point of the API.
newtype PrioritiesResponse = PrioritiesResponse [Priority]
  deriving (Eq, FromJSON, Show)

-- | Type representing neighbourhood priorities.
data Priority = Priority
  { pIssue      :: Text
  , pIssueDate  :: Maybe LocalTime
  , pAction     :: Maybe Text
  , pActionDate :: Maybe LocalTime
  } deriving (Eq, Generic, Show)

instance FromJSON Priority where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = camelTo2 '_' . drop 1 }

-- | Type representing the response to an end point of the API.
data LocateNeighbourhoodResponse
  = LocateNeighbourhoodResponse Force NeighbourhoodId
  deriving (Eq, Show)

instance FromJSON LocateNeighbourhoodResponse where
  parseJSON = withObject "LocateNeighbourhoodResponse" $ \o -> do
    f   <- o .: "force"
    nId <- o .: "neighbourhood"
    pure $ LocateNeighbourhoodResponse f nId

-- | Type representing the response to an end point of the API.
newtype StopsStreetResponse = StopsStreetResponse [Stop]
  deriving (Eq, FromJSON, Show)

-- | Type representing stops and searches
data Stop = Stop
  { ssType                           :: StopType
  , ssRemovalOfMoreThanOuterClothing :: Maybe Bool
  , ssLocation                       :: Maybe StreetLocation
  , ssDatetime                       :: ZonedTime
  , ssGender                         :: Maybe Gender
  , ssAgeRange                       :: Maybe StopAgeRange
  , ssSelfDefinedEthnicity           :: Maybe Text
  , ssOfficerDefinedEthnicity        :: Maybe Text
  , ssLegislation                    :: Maybe Text
  , ssObjectOfSearch                 :: Maybe Text
  , ssOperation                      :: Maybe Bool
  , ssOperationName                  :: Maybe Text
  , ssOutcome                        :: Maybe StopOutcome
  , ssOutcomeId                      :: Maybe StopOutcomeId
  , ssOutcomeLinkedToObjectOfSearch  :: Maybe Bool
  } deriving (Generic, Show)

instance Eq Stop where
  (==) s1 s2 =
    ssType1 == ssType2
    && ssRemovalOfMoreThanOuterClothing1 == ssRemovalOfMoreThanOuterClothing2
    && ssLocation1 == ssLocation2
    && zonedTimeToUTC ssDatetime1 == zonedTimeToUTC ssDatetime2
    && ssGender1 == ssGender2
    && ssAgeRange1 == ssAgeRange2
    && ssSelfDefinedEthnicity1 == ssSelfDefinedEthnicity2
    && ssOfficerDefinedEthnicity1 == ssOfficerDefinedEthnicity2
    && ssLegislation1 == ssLegislation2
    && ssObjectOfSearch1 == ssObjectOfSearch2
    && ssOperation1 == ssOperation2
    && ssOperationName1 == ssOperationName2
    && ssOutcome1 == ssOutcome2
    && ssOutcomeId1 == ssOutcomeId2
    && ssOutcomeLinkedToObjectOfSearch1 == ssOutcomeLinkedToObjectOfSearch2
   where
    Stop ssType1
         ssRemovalOfMoreThanOuterClothing1
         ssLocation1
         ssDatetime1
         ssGender1
         ssAgeRange1
         ssSelfDefinedEthnicity1
         ssOfficerDefinedEthnicity1
         ssLegislation1
         ssObjectOfSearch1
         ssOperation1
         ssOperationName1
         ssOutcome1
         ssOutcomeId1
         ssOutcomeLinkedToObjectOfSearch1 = s1
    Stop ssType2
         ssRemovalOfMoreThanOuterClothing2
         ssLocation2
         ssDatetime2
         ssGender2
         ssAgeRange2
         ssSelfDefinedEthnicity2
         ssOfficerDefinedEthnicity2
         ssLegislation2
         ssObjectOfSearch2
         ssOperation2
         ssOperationName2
         ssOutcome2
         ssOutcomeId2
         ssOutcomeLinkedToObjectOfSearch2 = s2

instance FromJSON Stop where
  parseJSON = withObject "Stop" $ \o -> do
    ssType                           <- o .: "type"
    ssRemovalOfMoreThanOuterClothing <- o .: "removal_of_more_than_outer_clothing"
    ssLocation                       <- o .: "location"
    ssDatetime                       <- o .: "datetime"
    ssGender                         <- o .: "gender"
    ssAgeRange                       <- o .: "age_range"
    ssSelfDefinedEthnicity           <- o .: "self_defined_ethnicity"
    ssOfficerDefinedEthnicity        <- o .: "officer_defined_ethnicity"
    ssLegislation                    <- o .: "legislation"
    ssObjectOfSearch                 <- o .: "object_of_search"
    ssOperation                      <- o .: "operation"
    ssOperationName                  <- o .: "operation_name"
    ssOutcome                        <- o .:?? "outcome"
    outcomeObject                    <- o .: "outcome_object"
    ssOutcomeId                      <- outcomeObject .:?? "id"
    ssOutcomeLinkedToObjectOfSearch <- o .: "outcome_linked_to_object_of_search"
    pure $ Stop {..}

-- | Type representing the types of stops and searches.
data StopType
  = PersonSearch
  | VehicleSearch
  | PersonAndVehicleSearch
  deriving (Eq, Show)

instance FromJSON StopType where
  parseJSON = withText "StopType" $ \case
    "Person search"             -> pure PersonSearch
    "Vehicle search"            -> pure VehicleSearch
    "Person and Vehicle search" -> pure PersonAndVehicleSearch
    _ -> fail "Not a stop type"

-- | Type representing reported genders of people stopped and searched.
data Gender
  = Male
  | Female
  deriving (Eq, Show)

instance FromJSON Gender where
  parseJSON = withText "Gender" $ \case
    "Male"   -> pure Male
    "Female" -> pure Female
    _ -> fail "Not a gender used in stop and search reporting"

-- | Type representing reported age ranges of people stopped and searched.
data StopAgeRange
  = Years10To17
  | Years18to24
  | Years25to34
  | Years35Plus
  deriving (Eq, Show)

instance FromJSON StopAgeRange where
  parseJSON = withText "StopAgeRange" $ \case
    "10-17"   -> pure Years10To17
    "18-24"   -> pure Years18to24
    "25-34"   -> pure Years25to34
    "over 34" -> pure Years35Plus
    _ -> fail "Not an age range used in stop and search reporting"

-- | Type representing stop outcomes.
data StopOutcome
  = NoStopOutcome
  | NothingFound
  | StopOutcome Text
  deriving (Eq, Show)

instance FromJSON StopOutcome where
  parseJSON (Bool False) = pure NothingFound
  parseJSON v = (withText "StopOutcome" $ \t -> pure $ StopOutcome t) v

-- | Type representing stop and search outcomes
newtype StopOutcomeId = StopOutcomeId Text
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype StopsAtLocationResponse = StopsAtLocationResponse [Stop]
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype StopsNoLocationResponse = StopsNoLocationResponse [Stop]
  deriving (Eq, FromJSON, Show)

-- | Type representing the response to an end point of the API.
newtype StopsForceResponse = StopsForceResponse [Stop]
 deriving (Eq, FromJSON, Show)

(.:??)
  :: FromJSON a
  => Object
  -> Text
  -> Parser (Maybe a)
(.:??) obj key =
  case H.lookup key obj of
    Nothing -> pure Nothing
    Just v -> let v' = if v == String "" then Null else v
              in  liftParseJSON p (listParser p) v' <?> Key key
 where
  p = parseJSON
