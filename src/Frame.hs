module Frame where

import           Joint

data TimeCode = TimeCode
    { hr  :: Int
    , min :: Int
    , sec :: Int
    , frm :: Int
    }

data Frame = Frame
    { _num      :: Integer
    , _timecode :: TimeCode
    , _joints   :: [Joint]
    }
