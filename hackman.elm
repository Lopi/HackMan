module Hackman where

import Keyboard
import Window
import Debug

-- INPUT:

-- The information we need to represent all relevant user input.

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

--}}}

-- MODEL:

-- The information we need to represent the entire game.

-- {{{

type Object a       = { a | x:Float    -- x coordinate
                          , y:Float    -- y coordinate
                          , vx:Float   -- x velocity
                          , vy:Float } -- y velocity

type Rectangle a       = { a | x:Float   -- top left corner x coordinate
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

-- How the game steps from one state to another based on user input.

-- {{{

within : Player ->   -- The platform we're testing whether our player is within
         Platform -> -- Our glorious hero
         Bool        -- Whether our glorious hero is within the platform
within player platform =
      ((player.x > platform.x - 8)                 && -- we're not to the left
       (player.x < (platform.x + platform.w) + 8)) && -- or to the right
      ((player.y < platform.y + 8)                 && -- or on top
       (player.y > platform.y))                       -- or below
       -- Note: this is using 8 as a tolerance because it feels pretty good.
       --   Feel free to change this.

stepV : Float -> -- Our velocity vector
        Bool ->  -- Whether there is an lower-bound collision
        Bool ->  -- Whether there is an upper-bound collision
        Float    -- New velocity vector
stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - (abs v)
     | otherwise      -> v

stepObj : Time ->
          Object a -> -- Original object
          Object a    -- Object after time
stepObj t ({x,y,vx,vy} as obj) =
  { obj | x <- x + (vx * t)
        , y <- y + (vy * t) }

jump    {y} g h =
  if (any (within h) g.platforms)
    then { h | vy <- 5 }
    else h
gravity t   g h = if not (any (within h) g.platforms) then { h | vy <- h.vy - t/4 } else h
physics t   g h = { h | x <- h.x + t*h.vx , y <- max 0 (h.y + t*h.vy) }
walk    {x} g h = { h | vx <- toFloat x
                  , dir <- if | x < 0     -> "left"
                              | x > 0     -> "right"
                              | otherwise -> h.dir }

-- }}}

-- VIEW:

-- How the game state is displayed to the user.

-- {{{

-- CODE GOES HERE

-- }}}
