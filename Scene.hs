module Scene where
import Vector

data Ray = Ray {
    ray_origin :: Vector,
    ray_dir :: Vector
} deriving (Show)

data Color = Color { red :: !Float, green :: !Float, blue :: !Float } deriving (Show, Eq)

scaleColor :: Float -> Color -> Color
scaleColor f (Color r g b) = Color (f*r) (f*g) (f*b)

instance Num Color where
    (Color r1 g1 b1) + (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)
    (Color r1 g1 b1) - (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)
    (Color r1 g1 b1) * (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)
    abs _ = error "abs Color: undefined"
    signum _ = error "abs Color: undefined"
    fromInteger _ = error "fromInteger Color: undefined"

instance Ord Color where
    (Color r1 g1 b1) <= (Color r2 g2 b2) = maximum [r1, g1, b1] <= maximum [r2, g2, b2]

data Primitive = Sphere Vector Float | Triangle Vector Vector Vector deriving (Show, Eq);
data Material = Material { emissive :: Color, diffuse :: Color } deriving (Show);
newtype SceneObj = SceneObj (Primitive, Material) deriving (Show);
newtype RayIntersection = IsectAt (Float, SceneObj, Ray) deriving (Show)

scene :: [SceneObj]
scene = [
        -- balls in scene
        SceneObj (Sphere (Vector 0 (-1.5) (-4)) 1.0,
                  Material {emissive=Color 0.0 0.0 0.0, diffuse=Color 1.0 1.0 1.0}),

        SceneObj (Sphere (Vector 2 (-1) (-4)) 0.5,
                  Material {emissive=Color 0.0 1.0 0.0, diffuse=Color 1.0 1.0 1.0}),
        SceneObj (Sphere (Vector (-2) (-1) (-4)) 0.5,
                  Material {emissive=Color 1.0 0.0 0.0, diffuse=Color 1.0 1.0 1.0}),
        SceneObj (Sphere (Vector 0 0 (-4)) 0.5,
                  Material {emissive=Color 0.0 0.0 1.0, diffuse=Color 1.0 1.0 1.0}),
        -- floor
        SceneObj (Triangle (Vector (-100) (-2) 0)
                           (Vector (-100) (-2) (-100))
                           (Vector 100 (-2) 0),
                  Material {emissive=Color 0 0 0, diffuse=Color 1.0 1.0 1.0}),
        SceneObj (Triangle (Vector 100 (-2) 0)
                           (Vector (-100) (-2) (-100))
                           (Vector (100) (-2) (-100)),
                  Material {emissive=Color 0 0 0, diffuse=Color 1.0 1.0 1.0}),
        -- back wall
        SceneObj (Triangle (Vector (-100) (-2) (-10))
                           (Vector 100 100 (-10))
                           (Vector 100 (-2) (-10)),
                  Material {emissive=Color 0 0 0, diffuse=Color 1.0 1.0 1.0}),
        SceneObj (Triangle (Vector 100 100 (-10))
                           (Vector (-100) 100 (-10))
                           (Vector (-100) (-2) (-10)),
                  Material {emissive=Color 0 0 0, diffuse=Color 1.0 1.0 1.0}),
        -- light
        SceneObj (Sphere (Vector 0 8 (-4)) 3.0,
                  Material {emissive=Color 1.0 1.0 0.8, diffuse=Color 1.0 1.0 1.0})
    ]
