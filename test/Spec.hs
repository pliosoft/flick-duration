import Test.Hspec
import Test.QuickCheck
import Data.Duration
import Data.Semigroup
import Data.Maybe (fromJust)
import Data.Ratio

main :: IO ()
main = hspec $ do
    describe "Data.FlickDuration" $ do
        describe "Core" $ do
            it "should have correct flicks for a second" $
                durationFromSeconds 1
                    `shouldBe` (durationFromFlicks 705600000)
            it "microseconds should round correct" $
                durationFromMicroseconds (1 % 10)
                    `shouldBe` durationFromFlicks 70
            it "should do basic combinations" $
                (durationFromSeconds 12 <> durationFromHours 1 <> durationFromMinutes 12)
                    `shouldBe` (durationFromFlicks 3056659200000)
            it "should do basic convert to hms correctly" $
                durationAsHms (durationFromSeconds 12 <> durationFromHours 1 <> durationFromMinutes 12)
                    `shouldBe` (1, 12, 12, 0, 0)
            it "should wrap larger values" $
                durationAsHms (durationFromSeconds 1201 <> durationFromHours 2 <> durationFromMinutes 18012 <> durationFromMilliseconds 123947 <> durationFromMicroseconds 97435)
                    `shouldBe` (302, 34, 5, 44, 435)
            it "should convert correctly to microseconds" $ property $
                \x -> durationAsMicroseconds (durationFromMicroseconds (fromIntegral (x :: Integer)))
                    `shouldSatisfy` (\y -> abs (y - fromIntegral x) < 0.01)
            it "should convert correctly to milliseconds" $ property $
                \x -> durationAsMilliseconds (durationFromMilliseconds (fromIntegral (x :: Integer)))
                    `shouldBe` (fromIntegral x)
            it "should convert correctly to seconds" $ property $
                \x -> durationAsSeconds (durationFromSeconds (fromIntegral (x :: Integer)))
                    `shouldBe` (fromIntegral x)
            it "should convert correctly to minutes" $ property $
                \x -> durationAsMinutes (durationFromMinutes (fromIntegral (x :: Integer)))
                    `shouldBe` (fromIntegral x)
            it "should convert correctly to hours" $ property $
                \x -> durationAsHours (durationFromHours (fromIntegral (x :: Integer)))
                    `shouldBe` (fromIntegral x)
            it "should compute multiple durations" $
                durationOfRepeatedDuration (durationFromSeconds 12) 10
                    `shouldBe` durationFromSeconds (12 * 10)
            it "should not divide by zero" $
                durationFromDividingDuration (durationFromSeconds 12) 0
                    `shouldBe` Nothing
            it "durationIntoDuration" $
                durationIntoDuration (durationFromSeconds 1) (durationFromMicroseconds 100)
                    `shouldBe` Just 10000
            it "handles iterating zero duration" $
                durationIntoDuration (durationFromSeconds 1) (durationFromSeconds 0)
                    `shouldBe` Nothing
            it "should divide durations into parts" $
                durationFromDividingDuration (durationFromSeconds 12) 6
                    `shouldBe` (Just (durationFromSeconds 2))
            it "mconcat array of durations" $
                mconcat [durationFromMilliseconds 100, durationFromMilliseconds 200, durationFromMilliseconds 40, durationFromMilliseconds 60]
                    `shouldBe` durationFromMilliseconds 400
            it "compare durations with eps LT" $
                compareDurationsWithEpsilon (durationFromMilliseconds 1) (durationFromMilliseconds 100) (durationFromMilliseconds 110)
                    `shouldBe` LT
            it "compare durations with eps GT" $
                compareDurationsWithEpsilon (durationFromMilliseconds 1) (durationFromMilliseconds 100) (durationFromMilliseconds 11)
                    `shouldBe` GT
            it "compare durations with eps EQ" $
                compareDurationsWithEpsilon (durationFromMilliseconds 1) (durationFromMilliseconds 100) (durationFromMilliseconds 101)
                    `shouldBe` EQ
            it "min of two durations" $
                (min (durationFromMilliseconds 100) (durationFromMicroseconds 1000), max (durationFromMilliseconds 100) (durationFromMicroseconds 1000))
                    `shouldBe` (durationFromMicroseconds 1000, durationFromMilliseconds 100)

        describe "Sleep and Measure" $ do
            it "should sleep for about the right amount of time" $ do
                e <- measurementEpsilon
                b <- startMeasuring
                let specDelay = durationFromMilliseconds 120
                delayThreadByDuration specDelay
                duration <- durationSince b
                duration `shouldSatisfy` (\y -> EQ == compareDurationsWithEpsilon e specDelay y)

        describe "Frequency" $ do
            it "should not support negative bpm" $
                durationOfOneBeatAtBPM (-110)
                    `shouldBe` Nothing
            it "should compute correct duration of bpm" $
                durationOfOneBeatAtBPM 120
                    `shouldBe` Just (durationFromMilliseconds 500)
            it "should get the exact period and frequency of bpm for particular value" $
                (maybe Nothing frequencyInHzForWavePeriod (durationOfOneBeatAtBPM 100))
                    `shouldBe` (Just (100 % 60))
            it "should get the exact period and frequency of hz for particular value" $
                (maybe Nothing frequencyInHzForWavePeriod (durationOfOneCycleAtHz 100))
                    `shouldBe` (Just 100)
            it "should get the approx period and frequency for arbitrary hz" $ property $
                \x -> (maybe Nothing frequencyInHzForWavePeriod (durationOfOneCycleAtHz (fromIntegral (x :: Integer))))
                    `shouldSatisfy` (\y -> if x <= 0 then y == Nothing else let fy = fromJust y in abs (fromIntegral x - fy) < 1)
            it "should exclude zero hz" $
                durationOfOneCycleAtHz 0
                    `shouldBe` Nothing
            it "should compute correct duration of cycle" $ property $
                \x -> if x > 0 then durationOfOneCycleAtHz x `shouldBe` Just (durationFromSeconds (recip x)) else return ()
            it "should compute correct hz" $
                Just (durationFromMilliseconds 500)
                    `shouldBe` (durationOfOneCycleAtHz 2)

