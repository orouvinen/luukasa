module Render (render) where

import qualified AppState             as ST
import qualified Body                 as B
import qualified Joint                as J

import           Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import           Data.Bifunctor       (bimap)
import           Data.Foldable        (toList)
import           Data.IORef

import           GI.Cairo.Render      as CR hiding (x, y)
import qualified GI.Gtk               as Gtk

data Color = Color { r :: Double, g :: Double, b :: Double }

bgColor :: Color
bgColor = Color 0.15 0.15 0.15

jointColor :: Color
jointColor = Color 0.75 0.75 0.75

jointColorSelected :: Color
jointColorSelected = Color 1 0 0

limbColor :: Color
limbColor = Color 0.5 0.5 0.5

limbWidth :: Int
limbWidth = 2

jointRadius :: Int
jointRadius = 4

jointRadiusSelected  :: Int
jointRadiusSelected = 5

setSourceColor :: Color -> Render ()
setSourceColor color = CR.setSourceRGB (r color) (g color) (b color)


render :: IORef ST.AppState -> Render Bool
render state = do
    s <- liftIO $ readIORef state

    -- liftIO $ putStr $ "RENDER: " ++ ST.printState s
    runReaderT doRender s
    return True

doRender :: ReaderT ST.AppState Render ()
doRender = do
    body' <- asks ST.body
    viewScale' <- asks ST.viewScale
    viewTranslate' <- asks ST.viewTranslate
    let limbs = B.limbSegments body'
    let joints = toList (B.root body')

    lift $ do
        setSourceColor bgColor
        CR.paint

    mapM_ (\seg -> do
            lift CR.save >> doRenderTranslate viewTranslate' >> doRenderScale viewScale'
            renderLimb seg limbColor >> lift CR.restore)
        limbs

    mapM_ (\j -> do
            lift CR.save >> doRenderTranslate viewTranslate' >> doRenderScale viewScale'
            renderJoint j >> lift CR.restore)
        joints

doRenderTranslate :: (Int, Int) -> ReaderT ST.AppState Render ()
doRenderTranslate tr = do
    let (x, y) = bimap fromIntegral fromIntegral tr
    lift $ CR.translate x y

doRenderScale :: Double -> ReaderT ST.AppState Render ()
doRenderScale scaleFactor =
    lift $ CR.scale scaleFactor scaleFactor

renderLimb :: ((Double, Double), (Double, Double)) -> Color -> ReaderT ST.AppState Render ()
renderLimb limb color = do
    scaleFactor <- asks ST.viewScale

    let (x, y) = fst limb
    let (x', y') = snd limb
    lift $ do
        CR.moveTo x y
        CR.lineTo x' y'
        setSourceColor color
        CR.setLineWidth $ fromIntegral limbWidth / scaleFactor
        CR.stroke

renderJoint :: J.Joint -> ReaderT ST.AppState Render ()
renderJoint j = do
    st <- ask
    let scaleFactor = ST.viewScale st

    let isJointSelected = ST.isSelected st (J.jointId j)
    let radius = fromIntegral (if isJointSelected then jointRadiusSelected else jointRadius) / scaleFactor
    let (x, y) = (J.jointX j, J.jointY j)

    -- TODO: thin dashed line around selected joint
    lift $ do
        setSourceColor $ if isJointSelected then jointColorSelected else jointColor
        CR.arc x y radius 0 (pi * 2)
        CR.fill


