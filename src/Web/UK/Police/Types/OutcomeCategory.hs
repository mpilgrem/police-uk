{-|
Module      : Web.UK.Police.Types.OutcomeCategory
Description : Type representing outcome categories
Copyright   : (c) Mike Pilgrem 2019
License     : BSD 3-Clause
Maintainer  : public@pilgrem.com
Stability   : experimental

This module exports a type used to represent categories for the outcome of crime
reports used by the UK Home Office's Police API ('OutcomeCategory').
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.UK.Police.Types.OutcomeCategory
  ( OutcomeCategory (..)
  ) where

import Data.Aeson (FromJSON (parseJSON), withText)

-- | Type representing categories of outcomes for reported crimes.
data OutcomeCategory
  = AwaitingCourtResult
  | CourtResultUnavailable
  | UnableToProceed
  | LocalResolution
  | NoFurtherAction
  | DeprivedOfProperty
  | Fined
  | AbsoluteDischarge
  | Cautioned
  | DrugsPossessionWarning
  | PenaltyNoticeIssued
  | CommunityPenalty
  | ConditionalDischarge
  | SuspendedSentence
  | Imprisoned
  | OtherCourtDisposal
  | Compensation
  | SentencedInAnotherCase
  | Charged
  | NotGuilty
  | SentToCrownCourt
  | UnableToProsecute
  | FormalActionNotInPublicInterest
  | ActionTakenByAnotherOrganisation
  | FurtherInvestigationNotInPublicInterest
  | FurtherActionNotInPublicInterest
  | UnderInvestigation
  | StatusUpdateUnavailable
  deriving (Eq)

instance Show OutcomeCategory where
  show oc = case oc of
    AwaitingCourtResult                     -> "Awaiting court outcome"
    CourtResultUnavailable                  -> "Court result unavailable"
    UnableToProceed                         -> "Court case unable to proceed"
    LocalResolution                         -> "Local resolution"
    NoFurtherAction                         -> "Investigation complete; no suspect identified"
    DeprivedOfProperty                      -> "Offender deprived of property"
    Fined                                   -> "Offender fined"
    AbsoluteDischarge                       -> "Offender given absolute discharge"
    Cautioned                               -> "Offender given a caution"
    DrugsPossessionWarning                  -> "Offender given a drugs possession warning"
    PenaltyNoticeIssued                     -> "Offender given a penalty notice"
    CommunityPenalty                        -> "Offender given community sentence"
    ConditionalDischarge                    -> "Offender given conditional discharge"
    SuspendedSentence                       -> "Offender given suspended prison sentence"
    Imprisoned                              -> "Offender sent to prison"
    OtherCourtDisposal                      -> "Offender otherwise dealt with"
    Compensation                            -> "Offender ordered to pay compensation"
    SentencedInAnotherCase                  -> "Suspect charged as part of another case"
    Charged                                 -> "Suspect charged"
    NotGuilty                               -> "Defendant found not guilty"
    SentToCrownCourt                        -> "Defendant sent to Crown Court"
    UnableToProsecute                       -> "Unable to prosecute suspect"
    FormalActionNotInPublicInterest         -> "Formal action is not in the public interest"
    ActionTakenByAnotherOrganisation        -> "Action to be taken by another organisation"
    FurtherInvestigationNotInPublicInterest -> "Further investigation is not in the public interest"
    FurtherActionNotInPublicInterest        -> "Further action is not in the public interest"
    UnderInvestigation                      -> "Under investigation"
    StatusUpdateUnavailable                 -> "Status update unavailable"

instance FromJSON OutcomeCategory where
  parseJSON = withText "OutcomeCategory" $ \case
    "awaiting-court-result" -> pure AwaitingCourtResult
    "court-result-unavailable" -> pure CourtResultUnavailable
    "unable-to-proceed" -> pure UnableToProceed
    "local-resolution" -> pure LocalResolution
    "no-further-action" -> pure NoFurtherAction
    "deprived-of-property" -> pure DeprivedOfProperty
    "fined" -> pure Fined
    "absolute-discharge" -> pure AbsoluteDischarge
    "cautioned" -> pure Cautioned
    "drugs-possession-warning" -> pure DrugsPossessionWarning
    "penalty-notice-issued" -> pure PenaltyNoticeIssued
    "community-penalty" -> pure CommunityPenalty
    "conditional-discharge" -> pure ConditionalDischarge
    "suspended-sentence" -> pure SuspendedSentence
    "imprisoned" -> pure Imprisoned
    "other-court-disposal" -> pure OtherCourtDisposal
    "compensation" -> pure Compensation
    "sentenced-in-another-case" -> pure SentencedInAnotherCase
    "charged" -> pure Charged
    "not-guilty" -> pure NotGuilty
    "sent-to-crown-court" -> pure SentToCrownCourt
    "unable-to-prosecute" -> pure UnableToProsecute
    "formal-action-not-in-public-interest" -> pure FormalActionNotInPublicInterest
    "action-taken-by-another-organisation" -> pure ActionTakenByAnotherOrganisation
    "further-investigation-not-in-public-interest" -> pure FurtherInvestigationNotInPublicInterest
    "further-action-not-in-public-interest" -> pure FurtherActionNotInPublicInterest
    "under-investigation" -> pure UnderInvestigation
    "status-update-unavailable" -> pure StatusUpdateUnavailable
    "Awaiting court outcome"                              -> pure AwaitingCourtResult
    "Court result unavailable"                            -> pure CourtResultUnavailable
    "Court case unable to proceed"                        -> pure UnableToProceed
    "Local resolution"                                    -> pure LocalResolution
    "Investigation complete; no suspect identified"       -> pure NoFurtherAction
    "Offender deprived of property"                       -> pure DeprivedOfProperty
    "Offender fined"                                      -> pure Fined
    "Offender given absolute discharge"                   -> pure AbsoluteDischarge
    "Offender given a caution"                            -> pure Cautioned
    "Offender given a drugs possession warning"           -> pure DrugsPossessionWarning
    "Offender given a penalty notice"                     -> pure PenaltyNoticeIssued
    "Offender given community sentence"                   -> pure CommunityPenalty
    "Offender given conditional discharge"                -> pure ConditionalDischarge
    "Offender given suspended prison sentence"            -> pure SuspendedSentence
    "Offender sent to prison"                             -> pure Imprisoned
    "Offender otherwise dealt with"                       -> pure OtherCourtDisposal
    "Offender ordered to pay compensation"                -> pure Compensation
    "Suspect charged as part of another case"             -> pure SentencedInAnotherCase
    "Suspect charged"                                     -> pure Charged
    "Defendant found not guilty"                          -> pure NotGuilty
    "Defendant sent to Crown Court"                       -> pure SentToCrownCourt
    "Unable to prosecute suspect"                         -> pure UnableToProsecute
    "Formal action is not in the public interest"         -> pure FormalActionNotInPublicInterest
    "Action to be taken by another organisation"          -> pure ActionTakenByAnotherOrganisation
    "Further investigation is not in the public interest" -> pure FurtherInvestigationNotInPublicInterest
    "Further action is not in the public interest"        -> pure FurtherActionNotInPublicInterest
    "Under investigation"                                 -> pure UnderInvestigation
    "Status update unavailable"                           -> pure StatusUpdateUnavailable
    _ -> fail "Not an outcome category"
