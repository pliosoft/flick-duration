
{-|
Module      : Data.FlickDuration
Copyright   : (c) 2018 Christopher C Lord, Pliosoft
License     : BSD Style
Stability   : experimental
Maintainer  : christopher@pliosoft.com
Portability : GHC

Use a to contain some number of flicks, and provide standard numerical operations.

Based on idea from <https:www.facebook.com/christopher.horvath.395/posts/1157292757692660 ChristopherHorvath>
-}
module Data.Duration
    (
      -- * The 'Duration' type
      Duration

      -- * Constant Ratios
    , flicksPerMillisecond
    , flicksPerSecond
    , flicksPerMinute
    , flickRatio
    , durationToRationalFlicks
    , durationToRationalSeconds

    -- * 'Duration' to various human units
    , durationAsHours
    , durationAsNanoseconds
    , durationAsMicroseconds
    , durationAsMilliseconds
    , durationAsMinutes
    , durationAsSeconds
    , durationAsHms

    -- * Creating 'Duration' from various human units
    , durationFromFlicks
    , durationFromHours
    , durationFromNanoseconds
    , durationFromMicroseconds
    , durationFromMilliseconds
    , durationFromMinutes
    , durationFromSeconds

    -- * 'Duration' from wavelength at some frequency, and vice versa
    , durationOfOneBeatAtBPM
    , durationOfOneCycleAtHz
    , frequencyInHzForWavePeriod

    -- * 'Duration' manipulations
    , durationOfRepeatedDuration
    , durationFromDividingDuration
    , durationIntoDuration
    , negateDuration
    , compareDurationsWithEpsilon

    -- * Thread delay
    , delayThreadByDuration

    -- * Measuring delay with the system clock
    , StartOfDuration
    , startMeasuring
    , durationSince
    , measurementEpsilon
    )
where

import Prelude
import Data.Ratio
import Data.Semigroup
import Data.Maybe
import System.Clock
import Control.Concurrent(threadDelay)
import Control.Monad ((>>), when)

-- | Flicks are a small unit of time that are very evenly divisible by common file format time durations; i.e., for common durations they will have no rounding.
--   Even if there is rounding, it is a very short amount of time -- 1.417 nanoseconds, so rounding is typically not a concern.
flicksPerMillisecond :: Num a => a
flicksPerMillisecond = 705600

-- | How many flicks are in a second
flicksPerSecond :: Num a => a
flicksPerSecond = 705600000

-- | How many flicks are in a minute
flicksPerMinute :: Num a => a
flicksPerMinute = 60 * 705600000

-- | 'Ratio' of seconds to flicks. for example, 100 seconds of flicks is @(100 * flickRatio)@
flickRatio :: Fractional a => a
flickRatio = recip flicksPerSecond

-- | Represents a span of time using some number of flicks. To create a 'Duration' use:
--
-- * 'durationFromFlicks'
-- * 'durationFromHours'
-- * 'durationFromMicroseconds'
-- * 'durationFromMilliseconds'
-- * 'durationFromMinutes'
-- * 'durationFromSeconds'
--
-- 'Duration' can be concatinated with 'mappend' or the '(<>)' operator.
newtype Duration = Duration { fromDuration :: Integer }
    deriving (Eq, Ord, Show)

instance Semigroup Duration where
  a <> b = Duration $ fromDuration a + fromDuration b

instance Monoid Duration where
  mempty = Duration 0
  mappend = (<>)

{-
Can't have Num instance since it makes little sense to multiply durations Durations are not a ring. But we can have monoid (and group?) instances.
instance Num Duration where
  a + b = Duration $ fromDuration a + fromDuration b
  a * b = Duration $ fromDuration a * fromDuration b
  abs a = Duration $ abs (fromDuration a)
  signum a = Duration $ signum (fromDuration a)
  fromInteger a = Duration (fromInteger a)
  negate a = Duration (negate (fromDuration a))
-}

-- | Convert a 'Duration' into flicks, stored in a 'Rational'
durationToRationalFlicks :: Duration -> Rational
durationToRationalFlicks d = toRational (fromDuration d)

-- | Convert a 'Duration' into seconds, stored in a 'Rational'
durationToRationalSeconds :: Duration -> Rational
durationToRationalSeconds d = flickRatio * durationToRationalFlicks d

-- | given some 'Duration' d1, how many times can we iterate that duration into another duration
durationIntoDuration :: Duration -> Duration -> Maybe Rational
durationIntoDuration  _ d2 | fromDuration d2 == 0 = Nothing
durationIntoDuration d1 d2 = Just $ fromDuration d1 % fromDuration d2

-- | Repeat a 'Duration' some rational number of times to produce the total duration
durationOfRepeatedDuration :: Duration -> Rational -> Duration
durationOfRepeatedDuration d r = Duration $ truncate (durationToRationalFlicks d * r)

-- | take a 'Duration' and divide it into equal pieces of some length (the result)
durationFromDividingDuration :: Duration -> Rational -> Maybe Duration
durationFromDividingDuration d r = if r == 0 then Nothing else Just $ durationOfRepeatedDuration d (recip r)

-- | Given a number of flicks, produce a 'Duration'
durationFromFlicks :: Integral a => a -> Duration
durationFromFlicks i = Duration (fromIntegral i)

-- | Given a number of nanoseconds, produce a 'Duration'.
-- Note that flicks are larger than nanoseconds, so rounding will occur
durationFromNanoseconds :: Rational -> Duration
durationFromNanoseconds i = durationFromMicroseconds (i / 1000)

-- | Given a number of microseconds, produce a 'Duration'
durationFromMicroseconds :: Rational -> Duration
durationFromMicroseconds i = durationFromMilliseconds (i / 1000)

-- | Given a number of milliseconds, produce a 'Duration'
durationFromMilliseconds :: Rational -> Duration
durationFromMilliseconds i = Duration (truncate (i * flicksPerMillisecond))

-- | Given a number of seconds, produce a 'Duration'
durationFromSeconds :: Rational -> Duration
durationFromSeconds i = Duration (truncate (i * flicksPerSecond))

-- | Given a number of minutes, produce a 'Duration'
durationFromMinutes :: Rational -> Duration
durationFromMinutes i = Duration (truncate (i * flicksPerMinute))

-- | Given a number of hours, produce a 'Duration'
durationFromHours :: Rational -> Duration
durationFromHours i = durationFromMinutes (i * 60)

-- | Given an epsilon e and two durations, will determine their 'Ord' with respect to the epsilon
compareDurationsWithEpsilon :: Duration -> Duration -> Duration -> Ordering
compareDurationsWithEpsilon e a b =
    let diff = (fromDuration a) - (fromDuration b)
     in if abs diff <= fromDuration e
        then EQ
        else if diff < 0
             then LT
             else GT

-- | given some frequency in cycles per second, produce the 'Duration' of a single wavelength's period
durationOfOneCycleAtHz :: Rational -> Maybe Duration
durationOfOneCycleAtHz hz =
    if hz <= 0
    then Nothing
    else durationFromDividingDuration (durationFromSeconds 1) hz

-- | given some frequency in beats per minute, produce the 'Duration' between beats
durationOfOneBeatAtBPM :: Rational -> Maybe Duration
durationOfOneBeatAtBPM bpm =
    if bpm <= 0
    then Nothing
    else durationFromDividingDuration (durationFromMinutes 1) bpm

-- | If 'Duration' d is the time for one period of a wave, produce the corresponding frequency in hz.
frequencyInHzForWavePeriod :: Duration -> Maybe Rational
frequencyInHzForWavePeriod = durationIntoDuration (durationFromSeconds 1)

-- | given some 'Duration', represent it as a count of seconds
durationAsSeconds :: Duration -> Rational
durationAsSeconds d = durationAsMilliseconds d / 1000

-- | given some 'Duration', represent it as a count of minutes
durationAsMinutes :: Duration -> Rational
durationAsMinutes d = durationAsSeconds d / 60

-- | given some 'Duration', represent it as a count of hours
durationAsHours :: Duration -> Rational
durationAsHours d = durationAsMinutes d / 60

-- | given some 'Duration', represent it as a count of milliseconds
durationAsMilliseconds :: Duration -> Rational
durationAsMilliseconds d = fromDuration d % flicksPerMillisecond

-- | given some 'Duration', represent it as a count of microseconds
durationAsMicroseconds :: Duration -> Rational
durationAsMicroseconds d = fromRational 1000 * durationAsMilliseconds d

-- | given some 'Duration', represent it as a count of nanoseconds
durationAsNanoseconds :: Duration -> Rational
durationAsNanoseconds d = fromRational 1000 * durationAsMicroseconds d

-- | given some 'Duration' produce a negative version
negateDuration :: Duration -> Duration
negateDuration d = Duration (negate (fromDuration d))

-- | Given two durations, produce the larger of the two
maximumOfDurations :: Duration -> Duration -> Duration
maximumOfDurations d1 d2 =
    if d1 > d2 then d1 else d2

-- | Given two durations, produce the smaller of the two
minimumOfDurations :: Duration -> Duration -> Duration
minimumOfDurations d1 d2 =
    if d1 < d2 then d1 else d2

-- | present a 'Duration' as a count of hours, minutes, seconds, ms, us
durationAsHms :: Duration -> (Integer, Integer, Integer, Integer, Integer)
durationAsHms d =
    let (h,hr)   = fromDuration d `divMod` fromDuration (durationFromHours 1)
        (m,mr)   = hr `divMod` fromDuration (durationFromMinutes 1)
        (s,sr)   = mr `divMod` fromDuration (durationFromSeconds 1)
        (ms,msr) = sr `divMod` fromDuration (durationFromMilliseconds 1)
        (us,usr) = msr `divMod` fromDuration (durationFromMicroseconds 1)
     in (h,m,s,ms,us)

-- | Suspend currently running thread for a 'Duration' of time.
--
--   Provides no exact guarantee on when the thread will be resumed after the delay.
delayThreadByDuration :: Duration -> IO ()
delayThreadByDuration d = do
    let maxMicroseconds = durationFromMicroseconds (fromIntegral (maxBound :: Int))
    let w = min d maxMicroseconds
    threadDelay (truncate (durationAsMicroseconds (min d maxMicroseconds)))
    when (w /= d) (delayThreadByDuration (d <> negateDuration w))

-- | Holds implementation-defined start of time measurment for 'startMeasuring'
newtype StartOfDuration = StartOfDuration { toDurationFromOrigin :: Duration  }

-- | Helper for internal math
timespecToDuration :: TimeSpec -> Duration
timespecToDuration ts = durationFromSeconds (fromIntegral (sec ts)) <> durationFromNanoseconds (fromIntegral (nsec ts))

-- | start measuring a duration
startMeasuring :: IO StartOfDuration
startMeasuring = do
    now <- getTime Monotonic
    let d1 = timespecToDuration now
    return (StartOfDuration d1)

-- | How long since the start of measuring has it been, plus or minus 'measurementEpsilon'
durationSince :: StartOfDuration -> IO Duration
durationSince s = do
    now <- getTime Monotonic
    let d1 = toDurationFromOrigin s
    let d2 = timespecToDuration now
    return (Duration ((fromDuration d2) - (fromDuration d1)))

-- | About how accurate is are the methods 'durationSince' and 'startMeasuring'
measurementEpsilon :: IO Duration
measurementEpsilon = return (durationFromMilliseconds 100)

    {-  ugg, this does not seem to give a reasonable e
    do
       res <- getRes Monotonic
       let rres = timespecToDuration res
       putStrLn (show (durationToRationalSeconds rres))
       return $ rres
    -}

