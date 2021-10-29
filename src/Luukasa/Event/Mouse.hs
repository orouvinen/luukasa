module Luukasa.Event.Mouse where

import qualified GI.Gdk as Gdk

class Monad m => MouseEvent m where
    getScrollDirection :: Gdk.EventScroll -> m Gdk.ScrollDirection
    motionPos :: Gdk.EventMotion -> m (Double, Double)
    clickPos :: Gdk.EventButton -> m (Double, Double)
    clickModifiers :: Gdk.EventButton -> m [Gdk.ModifierType]
    motionModifiers :: Gdk.EventMotion -> m [Gdk.ModifierType]
