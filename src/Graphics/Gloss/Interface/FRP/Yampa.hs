module Graphics.Gloss.Interface.FRP.Yampa
    (playYampa, InputEvent)
  where

import           Control.Monad                    (when)
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           FRP.Yampa                        (Event (..), SF, react,
                                                   reactInit)
import           Graphics.Gloss                   (Color, Display, Picture,
                                                   blank)
import qualified Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G

type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided 
playYampa :: Display -- ^ The display method
          -> Color   -- ^ The background color
          -> Int     -- ^ The refresh rate, in Hertz
          -> SF (Event InputEvent) Picture
          -> IO ()
playYampa display color frequency mainSF = do
  picRef <- newIORef blank

  handle <- reactInit
    (return NoEvent)
    (\_ updated pic -> when updated (picRef `writeIORef` pic) >> return False)
    mainSF

  let delta = 0.01 / fromIntegral frequency

      -- An action to convert the world to a picture
      toPic = (const $ readIORef picRef)

      -- A function to handle input events
      handleInput = (\e t -> react handle (delta, Just (Event e)) >> return (t + delta))

      -- A function to step the world one  iteration. It is passed the period
      -- of time (in seconds) needing to be  advanced 
      stepWorld   = 
         (\d t -> let delta' = realToFrac d - t
                 in if delta' > 0
                      then react handle (delta', Just NoEvent) >> return 0.0
                      else return (-delta')) 

  playIO display color frequency 0 toPic handleInput stepWorld
