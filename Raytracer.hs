module Raytracer where
import Vector
--module Raytracer (Color(..), renderScene) where

epsilon :: Float
epsilon = 0.0000001

newtype RayIntersection = IsectAt Float deriving (Show, Eq, Ord)
data Color = Color { red :: Float, green :: Float, blue :: Float } deriving (Show, Eq)
instance Num Color where
    (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
    (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
    (Color r1 g1 b1) * (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)
    abs _ = error "abs Color: undefined"
    signum _ = error "abs Color: undefined"
    fromInteger _ = error "fromInteger Color: undefined"
instance Ord Color where
    (Color r1 g1 b1) <= (Color r2 g2 b2) = maximum [r1, g1, b1] <= maximum [r2, g2, b2]

maxColor :: Color -> Float
maxColor (Color r g b) = maximum [r, g, b]


data Primitive = Sphere Vector Float | Triangle Vector Vector Vector deriving (Show, Eq);

data Ray = Ray {
    ray_origin :: Vector,
    ray_dir :: Vector
} deriving (Show)


rayPrimitiveIntersects :: Ray -> Primitive -> Maybe RayIntersection
rayPrimitiveIntersects (Ray origin dir) (Sphere sphere_origin sphere_radius) =
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
                    if i1 < 0 then Just $ IsectAt i2
                    else Just $ IsectAt i1
                else
                    Nothing
        else
            Nothing
rayPrimitiveIntersects (Ray origin dir) (Triangle a b c) =
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
                    Just $ IsectAt dist
                else
                    Nothing
        else
            Nothing

findFirstIntersection :: Ray -> [Primitive] -> Maybe RayIntersection
findFirstIntersection ray primitives =
    findFirst $ filter (/= Nothing) $ map (rayPrimitiveIntersects ray) primitives
    where
    findFirst [] = Nothing
    findFirst xs = minimum xs

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

renderScene :: Int -> Int -> (Float, Float) -> [Color]
renderScene screenWidth screenHeight subPix =
    -- subPixX,Y jiggle the scene eye rays by fractions of a pixel in order to
    -- perform (multi-pass) anti-aliasing
    map rayColor [ findFirstIntersection ray primitives | ray <- eyeRays screenWidth screenHeight subPix ]
    where
        primitives = [
            Sphere (Vector 0 0 (-4)) 1.5,
            Sphere (Vector 2 0 (-4)) 1.5,
            Sphere (Vector 0 4 (-8)) 1.5,
            Triangle (Vector 0 0 (-4))
                     (Vector (-1) 1 (-2))
                     (Vector 0 1 (-4)),
            Triangle (Vector 0 0 (-4))
                     (Vector (-0.101) (-3) (-4))
                     (Vector (-0.1) (-3) (-4))]

        rayColor :: Maybe RayIntersection -> Color
        rayColor Nothing = Color 0 0 0
        rayColor (Just (IsectAt dist)) = Color (1.0/dist) (1.0/dist) (1.0/dist)
