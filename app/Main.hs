{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List.Extra (trim)
import Data.Ratio
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Web.FontAwesomeType

---------------
---- Types ----
---------------

data Battery = BAT0 | BAT1 deriving Show
data Attr = EnergyNowAttr | EnergyFullAttr | StatusAttr | PowerNowAttr

-- | ACPI Battery Properties
newtype EnergyNow  = EnergyNow  { getEnergyNow :: Integer }
newtype EnergyFull = EnergyFull { getEnergyFull :: Integer }
newtype PowerNow   = PowerNow   { getPowerNow :: Integer }
data ChargeStatus  = Charging | Discharging | Full deriving Eq

-- | Derived Battery Properties
newtype TimeRemaining = TimeRemaining { getTime :: DiffTime }
newtype EnergyPercent = EnergyPercent { getEnergyPercent :: Rational }

-- | Power Sources
data AcStatus = Connected | Disconnected deriving Eq
data BatteryStatus = BatteryStatus EnergyPercent ChargeStatus TimeRemaining


---------------------
---- TypeClasses ----
---------------------

instance Show Attr where
    show EnergyNowAttr  = "energy_now"
    show EnergyFullAttr = "energy_full"
    show StatusAttr     = "status"
    show PowerNowAttr   = "power_now"

instance Show EnergyPercent where
    show  energy@(EnergyPercent rat)
        | rat <= 1 % 8 = pure (fontAwesomeChar FaBatteryEmpty) ++ percent
        | rat <= 1 % 4 = pure (fontAwesomeChar FaBatteryQuarter) ++ percent
        | rat <= 1 % 2 = pure (fontAwesomeChar FaBatteryHalf) ++ percent
        | rat <= 3 % 4 = pure (fontAwesomeChar FaBatteryThreeQuarters) ++ percent
        | otherwise    = pure (fontAwesomeChar FaBatteryFull) ++ percent
        where percent = " " ++ show (calcPercent energy) ++ "%"

instance Show TimeRemaining where
    show = formatRemainingTime . getTime

instance Show AcStatus where
    show Disconnected = ""
    show Connected  = pure $ fontAwesomeChar FaPlug

instance Show BatteryStatus where
    show (BatteryStatus energy status timeRemaining) =
        let chargingIcon   = pure $ fontAwesomeChar FaBolt
        in if status == Charging
           then show energy ++ " " ++ chargingIcon
           else if status == Discharging
                then show energy ++ " " ++ show timeRemaining
                else show energy


--------------
---- Time ----
--------------

diffToNominal :: DiffTime -> NominalDiffTime
diffToNominal = fromRational . toRational

formatRemainingTime :: DiffTime -> String
formatRemainingTime = formatTime defaultTimeLocale "%H:%M" . posixSecondsToUTCTime . diffToNominal


--------------
---- Acpi ----
--------------

getAcpiAc :: IO AcStatus
getAcpiAc = do
    let sysFsPath = "/sys/class/power_supply/AC/online"
    ac <- (== "1") . trim <$> readFile sysFsPath
    if ac then return Connected else return Disconnected

getAcpiBat :: Battery -> IO BatteryStatus
getAcpiBat bat = do
    let sysFsPath = "/sys/class/power_supply/" ++ show bat ++ "/"
    energyNow  <- EnergyNow  . read       . trim <$> readFile (sysFsPath ++ show EnergyNowAttr)
    energyFull <- EnergyFull . read       . trim <$> readFile (sysFsPath ++ show EnergyFullAttr)
    status     <-              toStatus   . trim <$> readFile (sysFsPath ++ show StatusAttr)
    power      <- PowerNow   . read       . trim <$> readFile (sysFsPath ++ show PowerNowAttr)
    let energyPercent = toRatio energyNow energyFull
    let timeRemaining = TimeRemaining
                      . secondsToDiffTime
                      . (* 3600)
                      $ (getEnergyFull energyFull - getEnergyNow energyNow) `div` getPowerNow power

    return $ BatteryStatus energyPercent status timeRemaining

toStatus :: String -> ChargeStatus
toStatus str =
  case str of
      "Charging"    -> Charging
      "Discharging" -> Discharging
      _             -> Full

toRatio :: EnergyNow -> EnergyFull -> EnergyPercent
toRatio (EnergyNow now) (EnergyFull full) = EnergyPercent $ now % full

calcPercent :: EnergyPercent -> Integer
calcPercent = round @Double . (* 100) . realToFrac . getEnergyPercent

printStatus :: AcStatus -> BatteryStatus -> BatteryStatus -> IO ()
printStatus ac bat0@(BatteryStatus _ chargeStatus _) bat1@(BatteryStatus _ chargeStatus' _)
    | chargeStatus  /= Full = print bat0
    | chargeStatus' /= Full = print bat1
    | otherwise             = print ac

main :: IO ()
main = do
    ac   <- getAcpiAc
    bat0 <- getAcpiBat BAT0
    bat1 <- getAcpiBat BAT1
    print bat0
    printStatus ac bat0 bat1



data CracklePop = N Int | Crackle | Pop | CracklePop deriving Show

cracklepop :: [Int] -> [CracklePop]
cracklepop = fmap f
    where f :: Int -> CracklePop
          f x
            | x `mod` 3 == 0 && x `mod` 5 == 0 = CracklePop
            | x `mod` 3 == 0 = Crackle
            | x `mod` 5 == 0 = Pop
            | otherwise = N x

main' :: IO ()
main' = do
    let xs = cracklepop [1..100]
    mapM_ (\case
            (N i) -> print i
            x     -> print x) xs

crackle :: [String]
crackle = cycle $ replicate 2 [] ++ pure "Crackle"

pop :: [String]
pop = cycle $ replicate 4 [] ++ pure "Pop"

cracklepop' :: [String]
cracklepop' = zipWith3 f [1..100] crackle pop
    where f :: Int -> String -> String -> String
          f i ("") ("") = show i
          f _ ("") b = b
          f _ a ("") = a
          f _ a b    = a ++ b

main'' :: IO ()
main'' = mapM_ putStrLn cracklepop'
