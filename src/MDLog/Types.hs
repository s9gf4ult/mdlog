module MDLog.Types where

import Data.Scientific
import Data.Set as S
import Data.Text as T
import Data.Time
import Text.Parse.Units

newtype Tag = Tag
  { unTag :: Text
  } deriving (Eq, Ord, Show)

data SiUnit
  = Litre
  | Gram
  deriving (Eq, Ord, Show)

data SiPrefix
  = Kilo
  | Milli
  | Micro
  | Nano
  deriving (Eq, Ord, Show)

data Unit
  = Drop
  | MU
  -- ^ Metabolic units.
  | Expression (UnitExp SiPrefix SiUnit)
  -- ^ Arbitrary expression with SI units
  deriving (Eq, Ord, Show)

data Supplement = Supplement
  { _sTimestamp  :: ZonedTime
  , _sName       :: Text
  , _sDosageUnit :: Unit
  , _sDosage     :: Scientific
  } deriving (Eq, Ord, Show)

newtype Supplements = Supplements
  { unSupplements :: [Supplement]
  } deriving (Eq, Ord, Show)

data BioMetrics = BioMetrics
  { _bmWeight :: Maybe Scientific
  , _bmLength :: Maybe Scientific
  } deriving (Eq, Ord, Show)

-- | Arbitrary text with some notes about day and stuff.
newtype Note = Note
  { unNote :: Text
  } deriving (Eq, Ord, Show)

-- | Value from 0 to 10
newtype Assessment = Assessment
  { unAssessment :: Scientific
  } deriving (Eq, Ord, Show)

data Disease = Disease
  { _dName                 :: Text
  , _dSubjectiveAssessment :: Assessment
  -- ^ Assessment of disease. 0 is very good (absolutely healthy), 10
  -- is very bad (absolutely ill)
  , _dNote                 :: Note
  }

newtype Diseases = Diseases
  { unDiseases :: [Disease]
  } deriving (Eq, Ord, Show)

data Analyze = Analyze

newtype Analyzes = Analyzes
  { unAnalyzes :: [Analyze]
  } deriving (Eq, Ord, Show)

data LogRecord = LogRecord
  { _lrTime        :: ZonedTime
  , _lrTags        :: Set Tag
  , _lrSupplements :: Supplements
  , _lrBioMetrics  :: BioMetrics
  , _lrDiseases    :: Diseases
  , _lrAnalyzes    :: Analyzes
  , _lrNote        :: Note
  } deriving (Eq, Ord, Show)
