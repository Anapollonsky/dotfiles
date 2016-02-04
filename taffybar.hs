import System.Taffybar

import Control.Applicative
import Control.Exception

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.Battery
import System.Taffybar.NetMonitor

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingLabel

import System.Information.Memory
import System.Information.CPU

import System.Process
-- import Graphics.UI.Gtk
import Data.List

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

main = do
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew wcfg 10
      mpris = mprisNew defaultMPRISConfig
      mem = pollingGraphNew memCfg 1 memCallback
      net = netMonitorNewWith 0.5 "enp4s0" 2 (formatNetworkInfo defaultNetFormat)
      -- wifi = netMonitorNewWith 0.5 "wlp5s0" 2 (formatNetworkInfo defaultNetFormat)
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      batBar = batteryBarNew batBarCfg 1
  defaultTaffybar defaultTaffybarConfig {startWidgets = [pager, note]
                                        ,endWidgets = [batBar, tray, wea, clock, mem, cpu, mpris, net]
                                        }

memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                            , graphLabel = Just "mem"
                            }

cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                , (1, 0, 1, 0.5)
                                                ]
                            , graphLabel = Just "cpu"
                            }

wcfg = (defaultWeatherConfig "KMSN") {
                              weatherTemplate = "$tempC$ C"
                            }

batBarCfg = (defaultBarConfig colorFunc)
    { barDirection = HORIZONTAL
    , barWidth = 48
    }
  where
    colorFunc pct
      | pct < 0.2 = (1.0, 0.0, 0.0)
      | pct < 0.3 = (0.8, 0.4, 0.0)
      | pct < 0.5 = (0.5, 0.5, 0.0)
      | pct < 0.7 = (0.3, 0.7, 0.0)
      | pct == 1  = (0.5, 0.5, 1.0)
      | otherwise = (0.0, 1.0, 0.0)

formatNetworkInfo :: String -> String
formatNetworkInfo str = "<span color='yellow'>" ++ str ++ "</span>"
