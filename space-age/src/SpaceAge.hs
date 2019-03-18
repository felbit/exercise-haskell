module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

earthYear :: Float
earthYear = 31557600

scaleOn :: Planet -> Float
scaleOn Mercury = 0.2408467 
scaleOn Venus   = 0.61519726
scaleOn Earth   = 1.0
scaleOn Mars    = 1.8808158 
scaleOn Jupiter = 11.862615 
scaleOn Saturn  = 29.447498 
scaleOn Uranus  = 84.016846 
scaleOn Neptune = 164.79132 

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (scaleOn planet * earthYear)

