module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = (/) seconds $ yearsToSec $ earthYears planet

earthYears :: Planet -> Float
earthYears Mercury = 0.2408467
earthYears Venus = 0.61519726 
earthYears Earth = 1.0
earthYears Mars = 1.8808158 
earthYears Jupiter = 11.862615 
earthYears Saturn = 29.447498 
earthYears Uranus = 84.016846 
earthYears Neptune = 164.79132 

earthSeconds :: Float
earthSeconds = 31557600


yearsToSec :: Float -> Float
yearsToSec earthyears = (*) earthyears $ earthSeconds