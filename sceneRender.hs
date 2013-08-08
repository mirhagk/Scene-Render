import Data.List
import PNG
import Data.Array
import System.Cmd
import System.Info
import System.IO

drawscene s x y w h xinc yinc
	| h > 0 = drawsceneline s x y w xinc : drawscene s x (y+yinc) w (h-yinc) xinc yinc 
	| otherwise = []

drawsceneline s x y w xinc
	| w > 0 = drawscenepiece s x y : drawsceneline s (x+xinc) y (w-xinc) xinc
	| otherwise = []

scene
  :: (Num b) => [(Double -> Double -> Bool,
       (b, b, b, b, b))]
scene = [(circle 10 10 5, (1,0,0,1,0))]

test = writeImage (drawscene scene 0 0 20 20 1 1)

circle :: (Num a, Ord a,Floating a) => a -> a -> a -> a -> a -> Bool
circle xc yc r x y = (x-xc)**2 + (y-yc)**2 < r
	
--draw :: (R->R->B)
draw :: (Num a, Num b) => a -> a -> (a -> a -> Bool, (b, b, b, b, b)) -> (b, b, b, b, b)
draw x y (s,c) = if s x y then c else (0,0,0,0,-1)
	
orderByZ (_,_,_,_,z1) (_,_,_,_,z2) = compare z1 z2

removeUnseen :: (Num a, Ord a) => [(t, t1, t2, t3, a)] -> [(t, t1, t2, t3)]
removeUnseen ((r,g,b,a,z):[])
	| z>=0 = (r,g,b,a):[]
	| otherwise = []
removeUnseen ((r,g,b,a,z):rest)
	| z>=0 = (r,g,b,a): removeUnseen rest
	| otherwise = removeUnseen rest

drawscenepiece :: (Num a, Ord b, Num b) => [(a -> a -> Bool, (b, b, b, b, b))] -> a -> a -> (b, b, b)
drawscenepiece s x y = calcColour $ removeUnseen $ sortBy orderByZ $ map (draw x y) s
--drawscenepiece :: (Num a, Ord a) => [(a -> a -> Bool, (a, a, a, a, a))] -> a -> a -> [(a, a, a, a)]
--drawscenepiece s x y = removeUnseen $ sortBy orderByZ $ map (draw x y) s

--dsp:: (Num a, Ord a) => [(a -> a -> Bool, (a, a, a, a, a))] -> a -> a -> (a, a, a)
--dsp s x y = calcColour $ drawscenepiece s x y

calcColour :: (Num a) => [(a,a,a,a)] -> (a,a,a)
calcColour [] = (0,0,0)
calcColour ((r1,g1,b1,a):rest) = (r1*a + r2*(1-a),g1*a+g2*(1-a),b1*a+b2*(1-a))
			where (r2,b2,g2) = calcColour rest

convertCol :: (RealFrac a, Num a) => (a,a,a)->(Int,Int,Int)
convertCol (r,g,b) = (round (r*255), round (g*255), round (b*255))
convertGS :: (Int,Int,Int) -> Int
convertGS (r,g,b) = div (r+g+b) 3
convertImage x = convertGS (convertCol x)


writeImage scene = do writePNG "Image.png" dat
   where
      dat = map (map (convertImage)) scene

launchImage = case os of
                   "darwin" -> system "open Image.png" -- Apple
                   "linux"  -> system "eog Image.png"  -- Linux 
                   _        -> system "Image.png"      -- Windows


