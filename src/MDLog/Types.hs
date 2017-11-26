module MDLog.Types where

import Data.Scientific
import Data.Set as S
import Data.Text as T
import Data.Time
import Text.Parse.Units


newtype Time = Time
  { unTime :: ZonedTime
  } deriving (Show)

instance Eq Time where
  (Time a) == (Time b) = zonedTimeToUTC a == zonedTimeToUTC b
instance Ord Time where
  compare (Time a) (Time b) = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

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

deriving instance (Eq a, Eq b) => Eq (UnitExp a b)
deriving instance (Ord a, Ord b) => Ord (UnitExp a b)

newtype Si = Si
  { unSi :: UnitExp SiPrefix SiUnit
  } deriving (Eq, Ord, Show)

data Unit
  = Drop
  | MU
  -- ^ Metabolic units.
  | Expression Si
  -- ^ Arbitrary expression with SI units
  | SomeUnit Text
  -- ^ Arbitrary unit not presentable by given unit constants
  deriving (Eq, Ord, Show)

data Supplement = Supplement
  { _sTimestamp  :: Time
  , _sName       :: Text
  , _sDosageUnit :: Unit
  , _sDosage     :: Scientific
  } deriving (Eq, Ord, Show)

newtype Supplements = Supplements
  { unSupplements :: [Supplement]
  } deriving (Eq, Ord, Show)

data BioMetrics = BioMetrics
  { _bmWeight :: Maybe Scientific
  , _bmGrowth :: Maybe Scientific
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
  } deriving (Eq, Ord, Show)

newtype Diseases = Diseases
  { unDiseases :: [Disease]
  } deriving (Eq, Ord, Show)

data Analyze = Analyze
  deriving (Eq, Ord, Show)

newtype Analyzes = Analyzes
  { unAnalyzes :: [Analyze]
  } deriving (Eq, Ord, Show)

data LogRecord = LogRecord
  { _lrTime        :: Time
  , _lrTags        :: Set Tag
  , _lrSupplements :: Supplements
  , _lrBioMetrics  :: BioMetrics
  , _lrDiseases    :: Diseases
  , _lrAnalyzes    :: Analyzes
  , _lrNote        :: Note
  } deriving (Eq, Ord, Show)
