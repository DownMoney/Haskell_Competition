import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment (getArgs)
import Data.IORef

xrot = (Vector3 (1::GLfloat) 0 0)

yrot = (Vector3 0 (1::GLfloat) 0)

zrot = (Vector3 0 0 (1::GLfloat))

rotAngle :: GLfloat
rotAngle = 1.2

aniStep :: GLfloat
aniStep = 0.001

animation :: Int
animation = 5

x :: GLfloat 
x = 0.1

y :: GLfloat
y = 0.1

{- a :: GLfloat -> GLfloat
 a s = -0.966918 + s

b :: GLfloat -> GLfloat
b s = 2.879879 + s

c :: GLfloat -> GLfloat
c s = 0.765145 + s

d :: GLfloat -> GLfloat
d s = 0.744728 + s
-}

a :: GLfloat -> GLfloat
a s = -1.562918 + s

b :: GLfloat -> GLfloat
b s = 2.283879 + s

c :: GLfloat -> GLfloat
c s = 0.16914499 + s

d :: GLfloat -> GLfloat
d s = 0.14872801 + s

initialIterations :: Int
initialIterations = 100

iterations :: Int
iterations = 25000

compInitial :: GLfloat -> GLfloat -> Int -> GLfloat -> (GLfloat, GLfloat)
compInitial newX newY i h | i < initialIterations = compInitial (sin(newY*(b h)) + (c h)*sin(newX*(b h))) (sin(newX*(a h)) + (d h)*sin(newY*(a h))) (i+1) h
		 	| otherwise = (newX, newY)

compPoints :: GLfloat -> GLfloat -> Int -> GLfloat -> Int -> [(GLfloat, GLfloat, GLfloat)]
compPoints newX newY i h m | i < m =((sin(newY*(b h)) + (c h)*sin(newX*(b h))), (sin(newX*(a h)) + (d h)*sin(newY*(a h))), newX*newY) :  compPoints (sin(newY * (b h)) + (c h)*sin(newX*(b h))) (sin(newX*(a h)) + (d h)*sin(newY*(a h))) (i+1) h m

			| otherwise = [((sin(newY*(b h)) + (c h)*sin(newX*(b h))), (sin(newX*(a h)) + (d h)*sin(newY*(a h))), newX*newY)]

myPoints :: GLfloat -> Int -> [(GLfloat,GLfloat,GLfloat)]
myPoints h m = compPoints (fst p) (snd p) 0 h m where
	p = compInitial x y 0 h

main :: IO()
main = do 
  args <- getArgs
  (progname, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]    
  initialWindowSize $= Size 800 800
  createWindow "Michael Lotkowski"

  animate <- newIORef (False::Bool)
  step <- newIORef (0::GLfloat)

  keyboardMouseCallback $= Just (keyboardMouse animate step)
  displayCallback $= (display)
  mainLoop


-- KEYBOARD

keyboardMouse ani s key state modifiers position = do
  keyboardAct ani s key state

keyboardAct a step (Char 'a') Down = do 
  a' <- get a
  if a'== True then do
      a $= False;
      addTimerCallback 50 (highRes step)
    else do
      a $= True;
      addTimerCallback animation (timer step a rotAngle yrot) 

keyboardAct a step (Char 'h') Down = do   
  a $= False;
  addTimerCallback 50 (highRes step)

keyboardAct a step (Char '+') Down = do 
  s <- get step
  step $= s+1
  a $= False;
  addTimerCallback 50 (timer step a rotAngle yrot)

keyboardAct a step (Char '-') Down = do 
  s <- get step
  step $= s-1
  a $= False;
  addTimerCallback 50 (timer step a (-rotAngle) yrot)

keyboardAct a step (SpecialKey KeyLeft) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (-rotAngle) yrot)

keyboardAct a step (SpecialKey KeyRight) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (rotAngle) yrot)

keyboardAct a step (SpecialKey KeyDown) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (rotAngle) xrot)

keyboardAct a step (SpecialKey KeyUp) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (-rotAngle) xrot)

keyboardAct a step (SpecialKey KeyHome) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (rotAngle) zrot)

keyboardAct a step (SpecialKey KeyEnd) Down = do 
  a $= False;
  addTimerCallback 50 (timer step a (-rotAngle) zrot)


keyboardAct a step (Char 'i') Down = do 
  a $= False;
  step $= 0;
  addTimerCallback 50 (highRes step)

keyboardAct a step (SpecialKey KeyPageUp) Down = do 
  scale 1.1 1.1 (1.1::GLfloat);
  addTimerCallback 50 (timer step a 0 xrot)


keyboardAct a step (SpecialKey KeyPageDown) Down = do 
  scale 0.9 0.9 (0.9::GLfloat);
  addTimerCallback 50 (timer step a 0 xrot) 

keyboardAct a step (Char 's') Down = do 
  showInfo step
  

keyboardAct _ _ _ _ = return ()


-- TIMER

timer i ani angle rot = do
 clear [ColorBuffer]
 rotate angle $ rot

 i' <- get i

 renderPrimitive Points $ mapM_ (\(x, y, z)-> preservingMatrix $ do 
  color $ Color3 x y 1  
  vertex$Vertex3 x y z) (myPoints (i'*aniStep) iterations)
 
 a <- get ani

 if a==True then do 
  i$=i'+1;
  addTimerCallback animation (timer i ani rotAngle rot)
  else
    ani$=False

 swapBuffers


-- DISPLAY

display :: IO()
display  = do 
  clear [ColorBuffer]
 -- scale 0.7 0.7 (0.7::GLfloat)  
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  pointSmooth $= Enabled
  pointSize $= 1.0  
  renderPrimitive Points $ mapM_ (\(x, y, z)-> preservingMatrix $ do 
	color $ Color3 x y 1	
	vertex$Vertex3 x y z) (myPoints 0 100000)
  -- flush
  swapBuffers


-- HIGH RESOLUTION


highRes step = do
  clear [ColorBuffer]  
  s <- get step
  renderPrimitive Points $ mapM_ (\(x, y, z)-> preservingMatrix $ do 
  color $ Color3 x y 1    
  vertex$Vertex3 x y z) (myPoints (s*aniStep) 100000)
  -- flush
  swapBuffers


-- INFO
showInfo i = do 
   i' <- get i
   putStrLn ("a: " ++ show (a (i'*aniStep)));
   putStrLn ("b: " ++ show (b (i'*aniStep)));
   putStrLn ("c: " ++ show (c (i'*aniStep)));
   putStrLn ("d: " ++ show (d (i'*aniStep)));
   putStrLn (take 20 (repeat '-'))
