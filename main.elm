module Hackman where

import Keyboard
import Window

-- INPUT:

-- {{{

-- +---------------------------+
-- |       Keys Used           |
-- +---------+-----------------+
-- | KEY     | EFFECT          |
-- +---------+-----------------+
-- | [space] | up              |
-- | a       | left            |
-- | d       | right           |
-- | s       | down (Not used) |
-- +---------+-----------------+

input : Signal Input
input = let delta = lift (\t -> t/20) (fps 60)
                    in sampleOn delta (lift2 (,) delta (Keyboard.directions 32 83 65 68))

--}}}

-- MODEL:

-- {{{

(gameWidth,gameHeight) : (Int, Int)
(gameWidth,gameHeight) = Window.dimensions

type Object a       = { a | x:Float    -- x coordinate
                          , y:Float    -- y coordinate
                          , vx:Float   -- x velocity
                          , vy:Float } -- y velocity

type Rectangle         = { x:Float   -- top left corner x coordinate
                         , y:Float   -- top left corner y coordinate
                         , w:Float   -- width
                         , l:Float } -- length

type Player = Object {}

type Platform = Rectangle {}

data State = Play | Pause | Win

-- A game has a state, a player, and a number of platforms greater than or
--   equal to zero.
type Game = { state:State, player:Player, platforms:[Platform] }

defaultGame : Game
defaultGame =
  { state     = Pause
  , player    = { x  = 0
                , y  = 0
                , vx = 0
                , vy = 0}
  , platforms = []
  }

-- }}}

-- UPDATE:

-- {{{

within : Platform -> Player -> Bool
within platform player =
      ((player.x >= platform.x - 8)                 && -- we're not to the left
       (player.x <= (platform.x + platform.w) + 8)) && -- or to the right
      ((player.y <= platform.y + 8)                 && -- or on top
       (player.y >= (platform.y - platform.l) - 8))    -- or below
       -- Note: this is using 8 as a tolerance because it feels pretty good.
       --   Feel free to change this.

-- }}}
