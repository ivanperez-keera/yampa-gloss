{-# LANGUAGE Arrows           #-}
{-# LANGUAGE ParallelListComp #-}
import Control.Arrow                      ( returnA, (&&&), (>>^), (^<<) )
import FRP.Yampa                          ( SF, after, constant, switch, time )
import GHC.Float                          ( double2Float )
import Graphics.Gloss                     ( Color, Display (InWindow)
                                          , Picture (Color, Pictures, Translate)
                                          , aquamarine, azure, blue
                                          , chartreuse, circleSolid
                                          , cyan, green, magenta
                                          , orange, polygon, red, red
                                          , rose, rotate, thickArc
                                          , violet, white, withAlpha
                                          , yellow
                                          )
import Graphics.Gloss.Interface.FRP.Yampa ( playYampa )

main :: IO ()
main = playYampa (InWindow "YampaDemo" (1280, 1050) (200, 200)) white 30 rotatingColor

rotatingColor :: SF a Picture
rotatingColor = proc _ -> do
  t <- ftime -< () -- Yampa's time is Double, Gloss units are Float.
  returnA -< rotate (180 * t / pi) $ Translate 200 200
                                   $ Color (withAlpha 0.8 red)
                                   $ circleSolid 80

--
plainWave :: SF a Picture
plainWave = proc _ -> do
    t <- (*1.25) ^<< ftime -< ()

    let t2      n = (sin (t + 2 * pi * fromIntegral n / 45)) ** 2
        circleX n = fromIntegral n * 20 - 400
        circleY n = t2 n * 200

    returnA -< Pictures
      [ Translate (circleX n) (circleY n)
        $ Color (withAlpha 0.8 magenta)
        $ circleSolid 10
      | n <- [0 .. length colors]
      | c <- colors
      ]



wave :: SF a Picture
wave = proc _ -> do
  t  <- (*20) ^<< ftime -< ()
  let t2 = (t - 20) `modF` 60
  let t3 = (t - 40) `modF` 60
  let t4 = t `modF` 60
  let a1 = if t4 < 5 then Pictures [] else thickArc 0 90 t4 10
      a2 = if t2 < 5 then Pictures [] else thickArc 0 90 t2 10
      a3 = if t3 < 5 then Pictures [] else thickArc 0 90 t3 10
  returnA -< rotate 45 $ Pictures [a1, a2, a3]

linearTween :: Double -> SF a Double
linearTween maxTime =
    switch (progress &&& after maxTime ()) (\_ -> constant 1)
  where
    progress = proc _ -> do
      t <- time -< ()
      let prop = t / maxTime
      returnA -< prop

-- * Auxiliary definitions

-- ** Auxiliary Gloss definitions

thickRectangle thickness w h =
   Pictures [ polygon [(0,0), (w, 0), (w, thickness), (0, thickness), (0, 0)]
            , polygon [(w-thickness,0), (w, 0), (w, h), (w-thickness, h), (w-thickness, 0)]
            , polygon [(0,0), (thickness, 0), (thickness, h), (0, h), (0, 0)]
            , polygon [(0,h-thickness), (w, h-thickness), (w, h), (0, h), (0, h-thickness)]
            ]

colors :: [Color]
colors = [ red,  orange, yellow, chartreuse, green,   aquamarine
         , cyan, azure,  blue,   violet,     magenta, rose
         ]

arrowHead :: Picture
arrowHead = polygon [(100,0), (0, 40), (0, -40)]

nothing :: Picture
nothing = Pictures []

-- ** Auxiliary Num definitions

modF :: Float -> Float -> Float
modF f1 m = if f1 > m then modF (f1 - m) m else f1

-- ** Auxiliary Yampa definitions

untilSF  sf1 sf2 = sf1 &&& sf2
forSF    sf1 t   = sf1 `untilSF` after t ()
andThen_ sf1 sf2 = switch sf1 (\_ -> sf2)

ftime :: SF a Float
ftime = time >>^ double2Float
