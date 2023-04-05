-- |
-- Copyright  : (c) 2018-2023 Ivan Perez
--              (c) 2015-2018 Konstantin Saveljev
-- License    : MIT License (MIT)
-- Maintainer : ivan.perez@keera.co.uk
--
-- Gloss backend for Yampa.
--
-- Gloss is a purely functional library to create pictures and animations.
-- Yampa is a Functional Reactive Programming DSL structured around signal
-- functions.
--
-- This module provides a function to create an interactive Gloss animation
-- driven by a signal function that transforms a Gloss input signal into a
-- Gloss 'Picture'.
module Graphics.Gloss.Interface.FRP.Yampa
    (InputEvent, playYampa)
  where

-- External imports
import           Control.Monad                    (when)
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           FRP.Yampa                        (Event (..), SF, react,
                                                   reactInit)
import           Graphics.Gloss                   (Color, Display, Picture,
                                                   blank)
import           Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G

-- | Type representing input events to the signal function.
--
-- Note that this type represents the kind of information placed inside the
-- Yampa 'Event'. It will still be wrapped in an 'Event' to represent the fact
-- that an 'InputEvent' may or may not be present at one particular point in
-- time, and that it changes discretely.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided 
playYampa :: Display                       -- ^ The display method
          -> Color                         -- ^ The background color
          -> Int                           -- ^ The refresh rate, in Hertz
          -> SF (Event InputEvent) Picture -- ^ Signal function
          -> IO ()
playYampa display color frequency mainSF = do
  picRef <- newIORef blank

  handle <- reactInit
              (return NoEvent)
              (\_ updated pic -> do when updated (picRef `writeIORef` pic)
                                    return False
              )
              mainSF

  let delta = 0.01 / fromIntegral frequency

      -- An action to convert the world to a picture
      toPic = (const $ readIORef picRef)

      -- A function to handle input events
      handleInput e t = do react handle (delta, Just (Event e))
                           return (t + delta)

      -- A function to step the world one  iteration. It is passed the period
      -- of time (in seconds) needing to be  advanced 
      stepWorld   = 
        (\d t -> let delta' = realToFrac d - t
                 in if delta' > 0
                      then react handle (delta', Just NoEvent) >> return 0.0
                      else return (-delta')) 

  playIO display color frequency 0 toPic handleInput stepWorld
