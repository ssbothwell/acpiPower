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
newtype EnergyNow       = EnergyNow       { getEnergyNow       :: Integer }
newtype EnergyFull      = EnergyFull      { getEnergyFull      :: Integer }
newtype EnergyRemaining = EnergyRemaining { getEnergyRemaining :: Integer }
newtype PowerNow        = PowerNow        { getPowerNow        :: Integer }
data ChargeStatus       = Charging | Discharging | Full deriving Eq

-- | Derived Battery Properties
newtype TimeRemaining = TimeRemaining { getTime :: DiffTime }
newtype EnergyPercent = EnergyPercent { getEnergyPercent :: Rational }

-- | Power Sources
data AcStatus = Connected | Disconnected deriving Eq
data BatteryStatus = BatteryStatus Battery EnergyPercent ChargeStatus TimeRemaining


---------------------
---- TypeClasses ----
---------------------

instance Show Attr where
    showsPrec _ EnergyNowAttr  = showString "energy_now"
    showsPrec _ EnergyFullAttr = showString "energy_full"
    showsPrec _ StatusAttr     = showString "status"
    showsPrec _ PowerNowAttr   = showString "power_now"

instance Show EnergyPercent where
    showsPrec _ energy =
        let icon    = showString . pure . fontAwesomeChar . iconForBattery $ energy
            percent = showString (show $ fromEnergyPercent energy) . showString "%"
            space   = showString " "
        in icon . space . percent

instance Show TimeRemaining where
    show = formatRemainingTime . getTime

instance Show AcStatus where
    showsPrec _ Disconnected = showString "AC"
    showsPrec _ Connected    = showString (pure $ fontAwesomeChar FaPlug) . showString " AC"

instance Show BatteryStatus where
    showsPrec _ (BatteryStatus bat energy status timeRemaining) =
        let chargingIcon   = showString . pure $ fontAwesomeChar FaBolt
            bat'           = showString        $ show bat
            energy'        = showString        $ show energy
            timeRemaining' = showString        $ show timeRemaining
            space          = showString          " "
        in if status == Charging
           then bat' . space . energy' . space . chargingIcon
           else if status == Discharging
                then bat' . space . showString (show energy) . space . timeRemaining'
                else bat' . space . showString (show energy)


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
    status     <- toChargeStatus          . trim <$> readFile (sysFsPath ++ show StatusAttr)
    power      <- PowerNow   . read       . trim <$> readFile (sysFsPath ++ show PowerNowAttr)
    let energyPercent   = toEnergyPercent energyNow energyFull
    let energyRemaining = toEnergyRemaining energyNow energyFull
    let timeRemaining   = toTimeRemaining energyNow energyRemaining power

    return $ BatteryStatus bat energyPercent status timeRemaining

toChargeStatus :: String -> ChargeStatus
toChargeStatus str =
  case str of
      "Charging"    -> Charging
      "Discharging" -> Discharging
      _             -> Full

toEnergyRemaining :: EnergyNow -> EnergyFull -> EnergyRemaining
toEnergyRemaining (EnergyNow now) (EnergyFull full) = EnergyRemaining $ full - now

toTimeRemaining :: EnergyNow -> EnergyRemaining -> PowerNow -> TimeRemaining
toTimeRemaining now remain pow =
    let now' = fromInteger . getEnergyNow       $ now
        rem' = fromInteger . getEnergyRemaining $ remain
        pow' = fromInteger . getPowerNow        $ pow
    in TimeRemaining . secondsToDiffTime . round @Double . (* 3600) $ (now' - rem') / pow'

toEnergyPercent :: EnergyNow -> EnergyFull -> EnergyPercent
toEnergyPercent (EnergyNow now) (EnergyFull full) = EnergyPercent $ now % full

fromEnergyPercent :: EnergyPercent -> Integer
fromEnergyPercent = round @Double . (* 100) . realToFrac . getEnergyPercent

iconForBattery :: EnergyPercent -> FontAwesome
iconForBattery (EnergyPercent rat)
    | rat <= 1 % 8 = FaBatteryEmpty
    | rat <= 1 % 4 = FaBatteryQuarter
    | rat <= 1 % 2 = FaBatteryHalf
    | rat <= 3 % 4 = FaBatteryThreeQuarters
    | otherwise = FaBatteryFull

printStatus :: AcStatus -> BatteryStatus -> BatteryStatus -> IO ()
printStatus ac bat0@(BatteryStatus _ _ chargeStatus _) bat1@(BatteryStatus _ _ chargeStatus' _)
    | chargeStatus  /= Full = print bat0
    | chargeStatus' /= Full = print bat1
    | otherwise             = print ac

main :: IO ()
main = do
    ac   <- getAcpiAc
    bat0 <- getAcpiBat BAT0
    bat1 <- getAcpiBat BAT1
    printStatus ac bat0 bat1
