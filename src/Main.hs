module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Graphics.Gloss.Interface.IO.Interact



data House  = House Picture Float Float
data Player = Player Picture Float Float

data Ground = Ground Int -- height is the y value where ground starts
type Houses = [House]

data World = World Player Houses Ground

loadImages :: IO (Picture,  Picture)
loadImages = do
  hero <- (loadJuicyPNG $ imagePath "hero.png")
  house <- (loadJuicyPNG $ imagePath "home1.png")
  return (fromMaybe hero, fromMaybe house)

fromIO :: IO a -> a
fromIO  = unsafePerformIO

images :: (Picture, Picture)
images = fromIO loadImages

defaultPenguin :: Player
defaultPenguin = Player img x y
  where
    img = ((translate x y) . (scale scaleXY scaleXY)) (fst images)
    x = 0
    y = (-40)
    scaleXY = 2


defaultHouse :: House
defaultHouse = House img x y
  where
    img = ((translate x y) . (scale scaleXY scaleXY)) (snd images)
    x = 200
    y = 0
    scaleXY = 5



houses :: Houses
houses = [defaultHouse]


ground :: Ground
ground = Ground 0

fromMaybe :: Maybe Picture -> Picture
fromMaybe (Just a) = a
fromMaybe Nothing = Circle 30

imagePath :: String -> String
imagePath img = "/home/horhik/Pictures/Fun/" ++ img

-- | trueTranslate prevent translate function from creating recursive type Picture
-- | instead (Translate Float Float (Translate Float Floaat ... (Translate Float Float Picture))
-- | we'll get just (Translate Float Float Pictures) by compounding all Floats into one
trueTranslate :: Float -> Float -> Picture ->  Picture
trueTranslate newX newY (Translate x y sprite) = Translate (x + newX) (y + newY) sprite
trueTranslate x y picture = translate x y picture

movePlayer :: Float -> Float -> Player -> Player
movePlayer x y (Player sprite oldX oldY) = Player movedPicture newX newY
  where
    newX = x + oldX
    newY = y + oldY
    movedPicture = trueTranslate x y sprite

wtf :: Float -> World -> World
wtf _ world = world
wtf 0.5 (World p [(House sprite x y)] g) = World p [(House (trueTranslate hx hy sprite) newX newY )] g
  where
    hx = 10
    hy = 0
    newX = hx + x
    newY = hy + y

newWorld = World defaultPenguin houses ground

-- packPictures :: Picture -> Picture -> Picture
-- packPictures pictures = Pictures pictures

getHousePictures (House img _ _ ) = img

world2picture :: World -> Picture
world2picture (World (Player img1 _ _) imgs _ ) = Pictures $ img1 : (map getHousePictures imgs)

eventHandler :: Event -> World -> World
eventHandler (EventKey (SpecialKey KeyRight) _ _ _) (World player objs gr) = World (movePlayer 20 0 player) objs gr
eventHandler (EventKey (SpecialKey KeyLeft) _ _ _) (World player objs gr) = World (movePlayer (-20) 0 player) objs gr
eventHandler (EventKey (SpecialKey KeyUp) _ _ _) (World player objs gr) = World (movePlayer 0 20 player) objs gr
eventHandler (EventKey (SpecialKey KeyDown) _ _ _) (World player objs gr) = World (movePlayer 0 (-20) player) objs gr
eventHandler _ w = w

game world  = play FullScreen cyan 20 world world2picture eventHandler wtf

main :: IO ()
main = game newWorld
