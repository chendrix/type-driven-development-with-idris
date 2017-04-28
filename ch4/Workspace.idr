module Workspace

data B
  = F
  | T

data Direction
  = North
  | East
  | South
  | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

data Shape
  = Triangle Double Double
  | Rectangle Double Double
  | Circle Double
%name Shape shape, shape1, shape2

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle base height) = base * height
area (Circle radius) = pi * radius * radius


data Picture
  = Primitive Shape
  | Combine Picture Picture
  | Rotate Double Picture
  | Translate Double Double Picture
%name Picture pic, pic1, pic2

testPicture : Picture
testPicture = Combine (Translate 5 5 (Primitive (Rectangle 20 10))) (Combine (Translate 15 25 (Primitive (Triangle 10 10))) (Translate 35 5 (Primitive (Circle 5))))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic


data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing y = y
maxMaybe x Nothing = x
maxMaybe a@(Just x) b@(Just y) =
  if
    x >= y
  then
    a
  else
    b


biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle (Primitive x) = Nothing
biggestTriangle (Combine pic pic1) = maxMaybe (biggestTriangle pic) (biggestTriangle pic1)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))
