{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

import System.Environment (getArgs)

import Codec.Picture
import Codec.Picture.Types (Pixel8(..))
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))


type RGB8 = (Pixel8, Pixel8, Pixel8)


fromTutorial :: Int -> Int -> PixelRGB8
fromTutorial x y =
  let (q, r) = x `quotRem` max 10 y
      s = fromIntegral . min 0xff
  in PixelRGB8 (s q) (s r) (s (q + r + 30))


doublelToRGBImage :: Double -> RGB8
doublelToRGBImage l =
    let
        s = fromIntegral . min 0xff . round . (* 256)
    in
        (s l, s l, s l)


normalize :: Int -> Int -> Double
normalize x width =
    let
        xDouble = fromIntegral x
        halfWidthDouble = (fromIntegral width) / 2
    in
        (xDouble - halfWidthDouble) / halfWidthDouble


nonIntRem :: Double -> Double -> Double
nonIntRem x y =
    x - (y * (fromIntegral $ truncate (x / y)))


nonIntMod :: Double -> Double -> Double
nonIntMod x y =
    fromIntegral $ truncate (x / y)


field :: Double -> Double -> Double
field x y =
    let
        divisor = 0.05
        r = ((x ** 2 + y ** 2) ** 0.5)
    in
        1 - (r `nonIntMod` divisor) * divisor -- / divisor


functionWrapper :: (Int, Int) -> (Double -> Double -> Double) -> (Z :. Int :. Int) -> RGB8
functionWrapper (w, h) func (Z :. x :. y) =
    let
        xNormalized = normalize x w
        yNormalized = normalize y h
    in
        doublelToRGBImage $ func xNormalized yNormalized


generateImgRepa :: Int -> Int -> Array D DIM2 RGB8
generateImgRepa width height =
    R.fromFunction (Z :. width :. height) $ functionWrapper (width, height) field


-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))


-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r, g, b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b


main :: IO ()
main = do
    [path] <- getArgs
    let width = 1000
    let height = 1000
    img <- R.computeUnboxedP $ generateImgRepa width height
    (savePngImage path . ImageRGB8 . toImage) img
