module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy

data House  = House Picture Int Int
data Player = Player Picture Int Int

data Ground height= Ground height -- height is the y value where ground starts
data Houses = [House]
data Players = [Player]

data World = World Players Houses Ground

loadImages :: IO (Picture, Picture)
loadImages = do
  hero <- (loadJuicyPNG $ imagePath "hero.png")
  house <- (loadJuicyPNG $ imagePath "house1.png")
  return (hero, house)


penguin = Player ()

world []

fromMaybe (Just a) = a

imagePath :: String -> String
imagePath img = "/home/horhik/Pictures/Fun/" ++ img

packPictures :: Picture -> Picture -> Picture
packPictures pictures = Pictures pictures

eventHandler :: Event -> world -> world
eventHandler (EventKey KeyRight Down Modifiers (0,0) )
game world = play FullScreen black 60 world (wold2picture world) (events world) wtf

main :: IO ()
main = do
  png <- (loadJuicyPNG $ imagePath "home1.png")
  let imgs = packPictures (fromMaybe png) (color red $ text "LMAO")
  game imgs
