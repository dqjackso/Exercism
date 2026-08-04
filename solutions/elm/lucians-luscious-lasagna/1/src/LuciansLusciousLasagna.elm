module LuciansLusciousLasagna exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)

-- TODO: define the expectedMinutesInOven constant
expectedMinutesInOven = 40
timePerLayer = 2

-- TODO: define the preparationTimeInMinutes function
preparationTimeInMinutes numberOfLayers = timePerLayer * numberOfLayers

-- TODO: define the elapsedTimeInMinutes function
elapsedTimeInMinutes numberOfLayers numberOfMinutesInOven = 
    preparationTimeInMinutes numberOfLayers + numberOfMinutesInOven
