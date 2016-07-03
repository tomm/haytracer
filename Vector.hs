module Vector where

data Vector = Vector !Float !Float !Float deriving (Show, Eq, Ord)

instance Num Vector where
    (Vector ax ay az) + (Vector bx by bz) = Vector (ax + bx) (ay + by) (az + bz)
    (Vector ax ay az) - (Vector bx by bz) = Vector (ax - bx) (ay - by) (az - bz)
    _ * _ = error "(*) Vector: undefined"
    abs _ = error "abs Vector: undefined"
    signum _ = error "signum Vector: undefined"
    fromInteger i = Vector (fromInteger i) (fromInteger i) (fromInteger i)

fmul :: Float -> Vector -> Vector
m `fmul` (Vector ax ay az) = Vector (m*ax) (m*ay) (m*az)

imul :: (Integral a) => a -> Vector -> Vector
m `imul` (Vector ax ay az) = Vector (m'*ax) (m'*ay) (m'*az)
    where m' = fromIntegral m

dot :: Vector -> Vector -> Float
(Vector ax ay az) `dot` (Vector bx by bz) = ax*bx + ay*by + az*bz

cross :: Vector -> Vector -> Vector
(Vector ax ay az) `cross` (Vector bx by bz) = Vector (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

length :: Vector -> Float
length (Vector x y z) = sqrt(x*x + y*y + z*z)

normalize :: Vector -> Vector
normalize v@(Vector x y z) = Vector (x*rl) (y*rl) (z*rl)
    where rl = 1.0 / Vector.length v

data Matrix = Matrix Float Float Float Float
                     Float Float Float Float
                     Float Float Float Float
                     Float Float Float Float deriving (Show, Eq)

diag :: Float -> Float -> Float -> Float -> Matrix
diag a b c d = Matrix a 0 0 0
                      0 b 0 0
                      0 0 c 0
                      0 0 0 d

identity :: Matrix
identity = Matrix 1 0 0 0
                  0 1 0 0
                  0 0 1 0
                  0 0 0 1

mmulv :: Matrix -> Vector -> Vector
mmulv (Matrix m11 m12 m13 m14
              m21 m22 m23 m24
              m31 m32 m33 m34
              _ _ _ _) (Vector a b c) = Vector (m11*a + m12*b + m13*c + m14)
                                               (m21*a + m22*b + m23*c + m24)
                                               (m31*a + m32*b + m33*c + m34)

mmulm :: Matrix -> Matrix -> Matrix
mmulm (Matrix a11 a12 a13 a14
              a21 a22 a23 a24
              a31 a32 a33 a34
              a41 a42 a43 a44)
      (Matrix b11 b12 b13 b14
              b21 b22 b23 b24
              b31 b32 b33 b34
              b41 b42 b43 b44) =
       Matrix (a11*b11 + a12*b21 + a13*b31 + a14*b41)
              (a11*b12 + a12*b22 + a13*b32 + a14*b42)
              (a11*b13 + a12*b23 + a13*b33 + a14*b43)
              (a11*b14 + a12*b24 + a13*b34 + a14*b44)

              (a21*b11 + a22*b21 + a23*b31 + a24*b41)
              (a21*b12 + a22*b22 + a23*b32 + a24*b42)
              (a21*b13 + a22*b23 + a23*b33 + a24*b43)
              (a21*b14 + a22*b24 + a23*b34 + a24*b44)

              (a31*b11 + a32*b21 + a33*b31 + a34*b41)
              (a31*b12 + a32*b22 + a33*b32 + a34*b42)
              (a31*b13 + a32*b23 + a33*b33 + a34*b43)
              (a31*b14 + a32*b24 + a33*b34 + a34*b44)

              (a41*b11 + a42*b21 + a43*b31 + a44*b41)
              (a41*b12 + a42*b22 + a43*b32 + a44*b42)
              (a41*b13 + a42*b23 + a43*b33 + a44*b43)
              (a41*b14 + a42*b24 + a43*b34 + a44*b44)
