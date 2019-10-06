{-|
Module      : Web.UK.Police.Types.EWTForce
Description : Type representing territorial police forces in England and Wales
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports a type representing territorial police forces in England and
Wales ('EWTForce').
-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Web.UK.Police.Types.EWTForce where

import Data.Aeson (FromJSON (parseJSON), withText)
import Servant.API (ToHttpApiData, toUrlPiece)

{- | Type representing territorial police forces in England and Wales. There is
a nullary data constructor for each of the 43 territorial police forces.
-}
data EWTForce
  = AvonAndSomerset
  | Bedfordshire
  | Cambridgeshire
  | Cheshire
  | CityOfLondon
  | Cleveland
  | Cumbria
  | Derbyshire
  | DevonAndCornwall
  | Dorset
  | Durham
  | DyfedPowys
  | Essex
  | Gloucestershire
  | GreaterManchester
  | Gwent
  | Hampshire
  | Hertfordshire
  | Humberside
  | Kent
  | Lancashire
  | Leicestershire
  | Lincolnshire
  | Merseyside
  | Metropolitan
  | Norfolk
  | NorthWales
  | NorthYorkshire
  | Northamptonshire
  | Northumbria
  | Nottinghamshire
  | SouthWales
  | SouthYorkshire
  | Staffordshire
  | Suffolk
  | Surrey
  | Sussex
  | ThamesValley
  | Warwickshire
  | WestMercia
  | WestMidlands
  | WestYorkshire
  | Wiltshire
  deriving (Eq)

{- @show force@ returns the name of the territorial police force.
-}
instance Show EWTForce where
  show force = case force of
    AvonAndSomerset   -> "Avon and Somerset Constabulary"
    Bedfordshire      -> "Bedfordshire Police"
    Cambridgeshire    -> "Cambridgeshire Constabulary"
    Cheshire          -> "Cheshire Constabulary"
    CityOfLondon      -> "City of London Police"
    Cleveland         -> "Cleveland Police"
    Cumbria           -> "Cumbria Constabulary"
    Derbyshire        -> "Derbyshire Constabulary"
    DevonAndCornwall  -> "Devon & Cornwall Police"
    Dorset            -> "Dorset Police"
    Durham            -> "Durham Constabulary"
    DyfedPowys        -> "Dyfed-Powys Police"
    Essex             -> "Essex Police"
    Gloucestershire   -> "Gloucestershire Constabulary"
    GreaterManchester -> "Greater Manchester Police"
    Gwent             -> "Gwent Police"
    Hampshire         -> "Hampshire Constabulary"
    Hertfordshire     -> "Hertfordshire Constabulary"
    Humberside        -> "Humberside Police"
    Kent              -> "Kent Police"
    Lancashire        -> "Lancashire Constabulary"
    Leicestershire    -> "Leicestershire Police"
    Lincolnshire      -> "Lincolnshire Police"
    Merseyside        -> "Merseyside Police"
    Metropolitan      -> "Metropolitan Police Service"
    Norfolk           -> "Norfolk Constabulary"
    NorthWales        -> "North Wales Police"
    NorthYorkshire    -> "North Yorkshire Police"
    Northamptonshire  -> "Northamptonshire Police"
    Northumbria       -> "Northumbria Police"
    Nottinghamshire   -> "Nottinghamshire Police"
    SouthWales        -> "South Wales Police"
    SouthYorkshire    -> "South Yorkshire Police"
    Staffordshire     -> "Staffordshire Police"
    Suffolk           -> "Suffolk Constabulary"
    Surrey            -> "Surrey Police"
    Sussex            -> "Sussex Police"
    ThamesValley      -> "Thames Valley Police"
    Warwickshire      -> "Warwickshire Police"
    WestMercia        -> "West Mercia Police"
    WestMidlands      -> "West Midlands Police"
    WestYorkshire     -> "West Yorkshire Police"
    Wiltshire         -> "Wiltshire Police"

instance FromJSON EWTForce where
  parseJSON = withText "EWTForce" $ \case
    "avon-and-somerset"  -> pure AvonAndSomerset
    "bedfordshire"       -> pure Bedfordshire
    "cambridgeshire"     -> pure Cambridgeshire
    "cheshire"           -> pure Cheshire
    "city-of-london"     -> pure CityOfLondon
    "cleveland"          -> pure Cleveland
    "cumbria"            -> pure Cumbria
    "derbyshire"         -> pure Derbyshire
    "devon-and-cornwall" -> pure DevonAndCornwall
    "dorset"             -> pure Dorset
    "durham"             -> pure Durham
    "dyfed-powys"        -> pure DyfedPowys
    "essex"              -> pure Essex
    "gloucestershire"    -> pure Gloucestershire
    "greater-manchester" -> pure GreaterManchester
    "gwent"              -> pure Gwent
    "hampshire"          -> pure Hampshire
    "hertfordshire"      -> pure Hertfordshire
    "humberside"         -> pure Humberside
    "kent"               -> pure Kent
    "lancashire"         -> pure Lancashire
    "leicestershire"     -> pure Leicestershire
    "lincolnshire"       -> pure Lincolnshire
    "merseyside"         -> pure Merseyside
    "metropolitan"       -> pure Metropolitan
    "norfolk"            -> pure Norfolk
    "north-wales"        -> pure NorthWales
    "north-yorkshire"    -> pure NorthYorkshire
    "northamptonshire"   -> pure Northamptonshire
    "northumbria"        -> pure Northumbria
    "nottinghamshire"    -> pure Nottinghamshire
    "south-wales"        -> pure SouthWales
    "south-yorkshire"    -> pure SouthYorkshire
    "staffordshire"      -> pure Staffordshire
    "suffolk"            -> pure Suffolk
    "surrey"             -> pure Surrey
    "sussex"             -> pure Sussex
    "thames-valley"      -> pure ThamesValley
    "warwickshire"       -> pure Warwickshire
    "west-mercia"        -> pure WestMercia
    "west-midlands"      -> pure WestMidlands
    "west-yorkshire"     -> pure WestYorkshire
    "wiltshire"          -> pure Wiltshire
    _ -> fail "Unrecognised territorial force in England and Wales"

instance ToHttpApiData EWTForce where
  toUrlPiece f = case f of
    AvonAndSomerset   -> "avon-and-somerset"
    Bedfordshire      -> "bedfordshire"
    Cambridgeshire    -> "cambridgeshire"
    Cheshire          -> "cheshire"
    CityOfLondon      -> "city-of-london"
    Cleveland         -> "cleveland"
    Cumbria           -> "cumbria"
    Derbyshire        -> "derbyshire"
    DevonAndCornwall  -> "devon-and-cornwall"
    Dorset            -> "dorset"
    Durham            -> "durham"
    DyfedPowys        -> "dyfed-powys"
    Essex             -> "essex"
    Gloucestershire   -> "gloucestershire"
    GreaterManchester -> "greater-manchester"
    Gwent             -> "gwent"
    Hampshire         -> "hampshire"
    Hertfordshire     -> "hertfordshire"
    Humberside        -> "humberside"
    Kent              -> "kent"
    Lancashire        -> "lancashire"
    Leicestershire    -> "leicestershire"
    Lincolnshire      -> "lincolnshire"
    Merseyside        -> "merseyside"
    Metropolitan      -> "metropolitan"
    Norfolk           -> "norfolk"
    NorthWales        -> "north-wales"
    NorthYorkshire    -> "north-yorkshire"
    Northamptonshire  -> "northamptonshire"
    Northumbria       -> "northumbria"
    Nottinghamshire   -> "nottinghamshire"
    SouthWales        -> "south-wales"
    SouthYorkshire    -> "south-yorkshire"
    Staffordshire     -> "staffordshire"
    Suffolk           -> "suffolk"
    Surrey            -> "surrey"
    Sussex            -> "sussex"
    ThamesValley      -> "thames-valley"
    Warwickshire      -> "warwickshire"
    WestMercia        -> "west-mercia"
    WestMidlands      -> "west-midlands"
    WestYorkshire     -> "west-yorkshire"
    Wiltshire         -> "wiltshire"
