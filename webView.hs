import Control.Monad (void, join)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Game
import Data.IORef
import Graphics.UI.Threepenny
import Data.Foldable

main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "."} setup


mkSrow :: UI Element
mkSrow = UI.div #. "static-row"
            #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"]

mkSgrid :: UI Element
mkSgrid = UI.div #. "static-grid"
            #+ [mkSrow] #+ [mkSrow] #+ [mkSrow] #+ [mkSrow]


tileClass b (x,y) = "tile " ++ "pos-" ++ show x ++ "-" ++ show y ++ " val-" ++ show (b !! x !! y) ++ if (b !! x !! y) == 2 then " new" else ""

tilesChildren :: Board -> UI [Element]
tilesChildren board = do
                    let list = fmap (\(x,y) -> UI.div #. tileClass board (x,y) 
                            # set text (show (board !! x !! y))) (valTiles board)
                    sequence list


setup :: Window -> UI ()
setup w = do
    return w # set title "2048"
    UI.addStyleSheet w "game.css"

    startingBoard <- liftIO startBoard
    boardRef <- liftIO $ newIORef startingBoard

    -- Game Stuff
    staticGrid <- mkSgrid
    tiles <- UI.div #. "tiles-grid"

    gameGrid <- UI.div #. "game-grid"
        #+ [element staticGrid]
        #+ [element tiles]

    container <- UI.div #. "container"
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element gameGrid]

    getBody w #+ [element container]


    on UI.keydown container $ \c ->
        do
            board <- liftIO $ readIORef boardRef
            case getMove c of
                Just a -> do
                    nb <- liftIO $ turn board a
                    liftIO $ writeIORef boardRef nb
                    graphicBoard <- tilesChildren nb
                    element tiles #
                        set children graphicBoard
                Nothing -> element tiles
            --board <- liftIO $ day board



getMove :: KeyCode -> Maybe Move
getMove kc  | kc == 37 = Just LEFT
            | kc == 38 =  Just UP
            | kc == 39 = Just RIGHT
            | kc == 40 = Just DOWN
            | otherwise = Nothing

day :: IO Board -> IO ()
day b = do
    s <- b
    putStrLn (show s)