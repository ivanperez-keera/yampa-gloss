{-# LANGUAGE Arrows #-}
import Control.Arrow                      ( (^<<), returnA )
import FRP.Yampa                          ( SF, time )
import GHC.Float                          ( double2Float )
import Graphics.Gloss                     ( Picture (Color, Translate)
                                          , Display(InWindow), circleSolid
                                          , red, rotate, white, withAlpha )
import Graphics.Gloss.Interface.FRP.Yampa ( playYampa )

main :: IO ()
main = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30 rotatingColor

rotatingColor :: SF a Picture
rotatingColor = proc _ -> do
  t <- double2Float ^<< time -< () -- Yampa's time is Double, Gloss units are Float.
  returnA -< rotate (180 * t / pi) $ Translate 200 200
                                   $ Color (withAlpha 0.8 red)
                                   $ circleSolid 80
