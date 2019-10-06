{-|
Module      : Web.UK.Police.Types.CrimesCategory
Description : Type representing crimes categories
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports a type used to represent crime categories used by the UK
Home Office's Police API ('CrimesCategory').
-}
{-# LANGUAGE OverloadedStrings #-}

module Web.UK.Police.Types.CrimesCategory
  ( CrimesCategory (..)
  ) where

import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Text (Text, unpack)
import Servant.API (ToHttpApiData, toUrlPiece)

-- | Type representing crimes categories.
data CrimesCategory
  = AllCrime
  | AntiSocialBehaviour
  | BicycleTheft
  | Burglary
  | CriminalDamageArson
  | Drugs
  | OtherTheft
  | PossessionOfWeapons
  | PublicOrder
  | Robbery
  | Shoplifting
  | TheftFromThePerson
  | VehicleCrime
  | ViolentCrime
  | ViolenceAndSexualOffences
  | OtherCrime
  | UnknownCrimesCategory Text
  deriving (Eq)

instance Show CrimesCategory where
  show cc = case cc of
    AllCrime                  -> "All crime"
    AntiSocialBehaviour       -> "Anti-social behaviour"
    BicycleTheft              -> "Bicycle theft"
    Burglary                  -> "Burglary"
    CriminalDamageArson       -> "Criminal damage and arson"
    Drugs                     -> "Drugs"
    OtherTheft                -> "Other theft"
    PossessionOfWeapons       -> "Possession of weapons"
    PublicOrder               -> "Public order"
    Robbery                   -> "Robbery"
    Shoplifting               -> "Shoplifting"
    TheftFromThePerson        -> "Theft from the person"
    VehicleCrime              -> "Vehicle crime"
    ViolentCrime              -> "Violent crime"
    ViolenceAndSexualOffences -> "Violence and sexual offences"
    OtherCrime                -> "Other crime"
    UnknownCrimesCategory ccId -> "Unknown crimes category: " ++ unpack ccId

instance ToHttpApiData CrimesCategory where
  toUrlPiece cc = case cc of
    AllCrime                  -> "all-crime"
    AntiSocialBehaviour       -> "anti-social-behaviour"
    BicycleTheft              -> "bicycle-theft"
    Burglary                  -> "burglary"
    CriminalDamageArson       -> "criminal-damage-arson"
    Drugs                     -> "drugs"
    OtherTheft                -> "other-theft"
    PossessionOfWeapons       -> "possession-of-weapons"
    PublicOrder               -> "public-order"
    Robbery                   -> "robbery"
    Shoplifting               -> "shoplifting"
    TheftFromThePerson        -> "theft-from-the-person"
    VehicleCrime              -> "vehicle-crime"
    ViolentCrime              -> "violent-crime"
    ViolenceAndSexualOffences -> "violence-and-sexual-offences"
    OtherCrime                -> "other-crime"
    UnknownCrimesCategory ccId -> ccId

instance FromJSON CrimesCategory where
  parseJSON = withText "CrimesCategory" $ \t -> case t of
    "all-crime"                    -> pure AllCrime
    "anti-social-behaviour"        -> pure AntiSocialBehaviour
    "bicycle-theft"                -> pure BicycleTheft
    "burglary"                     -> pure Burglary
    "criminal-damage-arson"        -> pure CriminalDamageArson
    "drugs"                        -> pure Drugs
    "other-theft"                  -> pure OtherTheft
    "possession-of-weapons"        -> pure PossessionOfWeapons
    "public-order"                 -> pure PublicOrder
    "robbery"                      -> pure Robbery
    "shoplifting"                  -> pure Shoplifting
    "theft-from-the-person"        -> pure TheftFromThePerson
    "vehicle-crime"                -> pure VehicleCrime
    "violent-crime"                -> pure ViolentCrime
    "violence-and-sexual-offences" -> pure ViolenceAndSexualOffences
    "other-crime"                  -> pure OtherCrime
    _                              -> pure $ UnknownCrimesCategory t
