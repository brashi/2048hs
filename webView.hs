import Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Game hiding (main)

main :: IO ()
main = startGUI defaultConfig { jsStatic = Just "."} setup


mkSrow :: UI Element
mkSrow = UI.div #. "static-row"
            #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"] #+ [UI.div #. "grid-cell"]

mkSgrid :: UI Element
mkSgrid = UI.div #. "static-grid"
            #+ [mkSrow] #+ [mkSrow] #+ [mkSrow] #+ [mkSrow]

setup :: Window -> UI ()
setup w = do
    return w # set title "2048"
    UI.addStyleSheet w "game.css"

    let board = newBoard 4

    -- Game Stuff
    staticGrid <- mkSgrid

    gameGrid <- UI.div #. "game-grid"
        #+ [element staticGrid]

    container <- UI.div #. "container"
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element gameGrid]

    getBody w #+ [element container]


    on UI.keydown container $ \c ->
        do
            board <- pure (setValueAt board (0, 0) c)
            element gameGrid #+ [UI.div #. "tile"]