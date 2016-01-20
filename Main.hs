module Main where

import Graphics.UI.SDL.General as SDL.General
import Graphics.UI.SDL.Video as SDL.Video
import Graphics.UI.SDL.Types as SDL.Types
import Graphics.UI.SDL.Time as SDL.Time
import Graphics.UI.SDL.Events as SDL.Events
import Graphics.UI.SDL.Rect as SDL.Rect
import Graphics.UI.SDL.Color as SDL.Color
import qualified System.Exit
import qualified System.Random
import Data.Bits (shift)
import Control.Monad.State
import qualified Raytracer

screenWidth :: Int
screenWidth = 512
screenHeight :: Int
screenHeight = 512

exit :: IO ()
exit = SDL.General.quit >> System.Exit.exitSuccess

eventLoop :: IO ()
eventLoop = SDL.Events.pollEvent >>=
            \event -> case event of SDL.Events.NoEvent -> return ()
                                    SDL.Events.Quit -> exit
                                    _ -> eventLoop

pixelFromColor :: Raytracer.Color -> Pixel
pixelFromColor (Raytracer.Color r g b) =
    SDL.Color.Pixel (
        (truncate (255*r)) `shift` 16 +
        (truncate (255*g)) `shift` 8 +
        (truncate (255*b))
    )

raytraceAndRender :: Surface -> [Raytracer.Color] -> [Float] -> IO ([Float], [Raytracer.Color])
raytraceAndRender surface photonBuffer randFloats =
    do
        renderPixels 0 0 photonBuffer' $ \(Raytracer.Color r g b) ->
                                           Raytracer.Color (brightness*(log (1.0+r))) (brightness*(log (1.0+g))) (brightness*(log (1.0+b)))
        return (randFloats', photonBuffer')
    where
        -- subPix jiggle the scene eye rays by fractions of a pixel in order to
        -- perform multi-pass stochastic anti-aliasing
        (photons, randFloats') = runState (Raytracer.pathTraceScene screenWidth screenHeight) randFloats
        photonBuffer' = zipWith (+) photonBuffer photons
        maxColor = (Raytracer.maxColor . maximum) photonBuffer'
        brightness = 1.0 / (log (1.0+maxColor))

        renderPixels :: Int -> Int -> [Raytracer.Color] -> (Raytracer.Color -> Raytracer.Color) -> IO ()
        renderPixels x y colbuf postProcFn
            | y >= screenHeight = return ()
            | x < screenWidth = drawPix x y (head colbuf) postProcFn >> renderPixels (x+1) y (tail colbuf) postProcFn
            | x == screenWidth = renderPixels 0 (y+1) colbuf postProcFn

        drawPix :: Int -> Int -> Raytracer.Color -> (Raytracer.Color -> Raytracer.Color) -> IO Bool
        drawPix x y col postProcFn =
            SDL.Video.fillRect surface (Just $ SDL.Rect.Rect x y 1 1) $ (pixelFromColor . postProcFn) col

mainLoop :: [Float] -> [Raytracer.Color] -> Surface -> IO ()
mainLoop randFloats photonBuffer screen =
    do
        eventLoop
        (randFloats', photonBuffer') <- raytraceAndRender screen photonBuffer randFloats
        SDL.Video.flip screen
        SDL.Time.delay 100
        mainLoop randFloats' photonBuffer' screen

main :: IO ()
main = do
           SDL.General.init [SDL.General.InitVideo]
           screen <- SDL.Video.setVideoMode screenWidth screenHeight 32 [SDL.Types.SWSurface]
           mainLoop randFloats photonBuffer screen
       where
           randFloats = System.Random.randoms (System.Random.mkStdGen 123456)::[Float]
           photonBuffer = take (screenWidth*screenWidth) $ repeat $ Raytracer.Color 0 0 0
