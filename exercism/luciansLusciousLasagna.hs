module LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes) where

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven=40
-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes x = 2 * x
-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes x y = preparationTimeInMinutes x + y

