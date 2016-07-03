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
import qualified Scene

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

pixelFromColor :: Scene.Color -> Pixel
pixelFromColor (Scene.Color r g b) =
    SDL.Color.Pixel (
        (truncate (255*r)) `shift` 16 +
        (truncate (255*g)) `shift` 8 +
        (truncate (255*b))
    )

raytraceAndRender :: Surface -> [Scene.Color] -> [Float] -> IO ([Float], [Scene.Color])
raytraceAndRender surface photonBuffer randFloats =
    do
        renderPixels 0 0 photonBuffer' $ \(Scene.Color r g b) ->
                                           Scene.Color (brightness*sqrt(r))
                                                           (brightness*sqrt(g))
                                                           (brightness*sqrt(b))
        return (randFloats', photonBuffer')
    where
        -- subPix jiggle the scene eye rays by fractions of a pixel in order to
        -- perform multi-pass stochastic anti-aliasing
        (photons, randFloats') = runState (Raytracer.pathTraceScene screenWidth screenHeight) randFloats
        photonBuffer' = zipWith (+) photonBuffer photons
        maxColor = (Raytracer.maxColor . maximum) photonBuffer'
        brightness = 1.0 / sqrt(maxColor)

        renderPixels :: Int -> Int -> [Scene.Color] -> (Scene.Color -> Scene.Color) -> IO ()
        renderPixels x y colbuf postProcFn
            | y >= screenHeight = return ()
            | x == screenWidth = renderPixels 0 (y+1) colbuf postProcFn
            | otherwise = drawPix x y (head colbuf) postProcFn >> renderPixels (x+1) y (tail colbuf) postProcFn

        drawPix :: Int -> Int -> Scene.Color -> (Scene.Color -> Scene.Color) -> IO Bool
        drawPix x y col postProcFn =
            SDL.Video.fillRect surface (Just $ SDL.Rect.Rect x y 1 1) $ (pixelFromColor . postProcFn) col

mainLoop :: [Float] -> [Scene.Color] -> Surface -> IO ()
mainLoop randFloats photonBuffer screen =
    do
        eventLoop
        t <- SDL.Time.getTicks
        (randFloats', photonBuffer') <- raytraceAndRender screen photonBuffer randFloats
        t2 <- SDL.Time.getTicks
        putStrLn $ (show (t2 - t)) ++ " ms per frame, " ++ (show (512*512*1000 `div` (t2-t))) ++ " paths per second."
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
           photonBuffer = take (screenWidth*screenWidth) $ repeat $ Scene.Color 0 0 0
