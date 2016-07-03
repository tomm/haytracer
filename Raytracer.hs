module Raytracer where
import qualified Data.List
import qualified Data.Maybe
import Control.Monad.State
import Vector
import Scene
--import Debug.Trace
--module Raytracer (Color(..), renderScene) where

epsilon :: Float
epsilon = 0.0001

isectNormal :: RayIntersection -> Vector
isectNormal isect@(IsectAt(_, (SceneObj ((Sphere sphere_origin _), _)), _)) =
    normalize $ (isectPos isect) - sphere_origin

isectNormal (IsectAt (_, SceneObj (Triangle v1 v2 v3, _), _))  =
    normalize $ (v2-v1) `cross` (v2-v3)

isectPos :: RayIntersection -> Vector
isectPos (IsectAt (dist, _, Ray origin dir)) = origin + (dist `fmul` dir)

flipVectorToHemisphere :: Vector -> Vector -> Vector
flipVectorToHemisphere vec@(Vector x y z) norm =
    if vec `dot` norm > 0.0 then
        vec
    else
        Vector (-x) (-y) (-z)

{- ray bounces left, entropy pool, past intersections -}
type PathState = (Int, [Float], [RayIntersection])

randomVectorInHemisphere :: Vector -> State PathState Vector
randomVectorInHemisphere norm =
    do
        (bounces, randFloats, rayIsects) <- get
        put (bounces, drop 3 randFloats, rayIsects)
        return $ normalize $ flipVectorToHemisphere (
            Vector (0.5 - randFloats!!0) (0.5 - randFloats!!1) (0.5 - randFloats!!2)) norm


{- Make a random ray from the last ray intersection in PathState -}
newRandomRayFromLastIntersection :: State PathState Ray
newRandomRayFromLastIntersection =
    do
        (_, _, rayIsects) <- get
        let lastIsectNormal = isectNormal $ head rayIsects
        let rayStartPos = (isectPos $ head rayIsects) + (epsilon `fmul` lastIsectNormal)
        randDir <- randomVectorInHemisphere lastIsectNormal
        {-put (randFloats, rayIsects)-}
        return $ Ray rayStartPos randDir


{- Take an initial ray, trace it through scene, add its intersection data
   to PathState, and (if intersected) fire off new ray from intersected point
   until we've exhausted number of 'bounces' of PathState -}
makeRayScatterPath :: Ray -> [SceneObj] -> State PathState ()
makeRayScatterPath ray sceneobjs =
    case isect of Nothing -> return ()
                  Just isect' -> do
                      (bounces, randFloats, rayIsects) <- get
                      put (bounces-1, randFloats, isect':rayIsects)
                      nextRay sceneobjs (bounces-1)
    where
        isect = findFirstIntersection ray sceneobjs

        nextRay :: [SceneObj] -> Int -> State PathState ()
        nextRay sceneobjs' bounces
            | bounces > 0 = do
                            ray' <- newRandomRayFromLastIntersection
                            makeRayScatterPath ray' sceneobjs'
            | otherwise = return ()

        findFirstIntersection :: Ray -> [SceneObj] -> Maybe RayIntersection
        findFirstIntersection ray' objs =
                findFirst $ Data.Maybe.catMaybes $ map (rayPrimitiveIntersects ray') objs
            where
                findFirst :: [RayIntersection] -> Maybe RayIntersection
                findFirst [] = Nothing
                findFirst xs = Just $ Data.List.minimumBy isCloser xs

                isCloser :: RayIntersection -> RayIntersection -> Ordering
                isCloser (IsectAt (d1, _, _)) (IsectAt (d2, _, _)) =
                    if d1 < d2 then LT else GT


maxColor :: Color -> Float
maxColor (Color r g b) = maximum [r, g, b]

rayPrimitiveIntersects :: Ray -> SceneObj -> Maybe RayIntersection
rayPrimitiveIntersects ray@(Ray origin dir) obj@(SceneObj ((Sphere sphere_origin sphere_radius), _)) =
    let
        v = (origin - sphere_origin)
        b = -(v `dot` dir)
        sq_det = (b*b) - v `dot` v + sphere_radius*sphere_radius
    in
        if sq_det > 0 then
            let det = sqrt sq_det
                i1 = b - det
                i2 = b + det
            in
                if i2 > 0 then
                    if i1 < 0 then Just $ IsectAt (i2, obj, ray)
                    else Just $ IsectAt (i1, obj, ray)
                else
                    Nothing
        else
            Nothing
rayPrimitiveIntersects ray@(Ray origin dir) obj@(SceneObj ((Triangle a b c), _)) =
    let
        n = (c - a) `cross` (b - a)
        v0_cross = (b - origin) `cross` (a - origin)
        v1_cross = (a - origin) `cross` (c - origin)
        v2_cross = (c - origin) `cross` (b - origin)
        nominator = n `dot` (a - origin)
        v0d = v0_cross `dot` dir
        v1d = v1_cross `dot` dir
        v2d = v2_cross `dot` dir
    in
        if ( ((v0d > 0.0) && (v1d > 0.0) && (v2d > 0.0)) || 
             ((v0d < 0.0) && (v1d < 0.0) && (v2d < 0.0))) then
            let
                dist = nominator / (dir `dot` n)
            in
                if dist > epsilon then
                    {-texture coords!
                    vol = 1.0/(v0d+v1d+v2d)
                    v = v1d*vol 
                    u = v2d*vol -}
                    Just $ IsectAt (dist, obj, ray)
                else
                    Nothing
        else
            Nothing

-- Generate a 'pyramid' of eye rays from camera location toward the scene
-- Rays are returned in row-major order
eyeRays :: Int -> Int -> (Float, Float) -> [Ray]
eyeRays width height (subPixX, subPixY) =
    [ Ray (Vector 0 0 0) (normalize (topLeft + (x `imul` rightStep) + (y `imul` downStep)))
      | y <- [0..(height-1)], x <- [0..(width-1)]]
    where
        fw = fromIntegral width
        fh = fromIntegral height
        -- looking down -ve z axis
        aspect = (fw) / (fh) -- assumes square pixels
        topLeft' = Vector ((-1)*aspect) 1 (-1)
        rightStep = (1.0 / (fw-1)) `fmul` Vector (2*aspect) 0 0
        downStep = (1.0 / (fh-1)) `fmul` Vector 0 (-2) 0
        topLeft = topLeft' + subPixX `fmul` rightStep + subPixY `fmul` downStep

collectLightFromPath :: [RayIntersection] -> Color -> Color
collectLightFromPath [] color = color
collectLightFromPath (isect:isects) color =
    collectLightFromPath isects $ (emissive material) + reflected
    where
        surface_normal = isectNormal isect
        (IsectAt (_, sceneobj, ray)) = isect
        (SceneObj (_, material)) = sceneobj

        cos_theta = -(normalize $ ray_dir ray) `dot` surface_normal
        reflected = cos_theta `scaleColor` (color * (diffuse material))

pathTraceScene :: Int -> Int -> State [Float] [Color]
pathTraceScene screenWidth screenHeight = do
    -- subPixX,Y jiggle the scene eye rays by fractions of a pixel in order to
    -- perform (multi-pass) anti-aliasing
    randFloats <- get
    let (subpixX:subpixY:_) = randFloats
    put $ drop 2 randFloats
    pathTraceRays (eyeRays screenWidth screenHeight (subpixX, subpixY))
    where
        pathTraceRays :: [Ray] -> State [Float] [Color]
        pathTraceRays [] = return []
        pathTraceRays (ray:rays) = do
            randFloats' <- get
            let (_, (_, randFloats'', isects)) = runState (makeRayScatterPath ray scene)
                                                          (4, randFloats', [])
            put randFloats''
            -- XXX not tail recursion here!
            rest <- pathTraceRays rays
            return $ (collectLightFromPath isects (Color 0 0 0)):rest
