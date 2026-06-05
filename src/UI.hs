module UI where

import Ipair
import Brick
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border
import qualified Graphics.Vty as V

data WidgetName = Tilename Ipair | Background deriving (Eq, Ord, Show)
type WN = WidgetName

colorFG :: V.Color -> Widget WN -> Widget WN
colorFG c = modifyDefAttr (`V.withForeColor` c)

colorBG :: V.Color -> Widget WN -> Widget WN
colorBG c = modifyDefAttr (`V.withBackColor` c)

colorBoth :: V.Color -> Widget WN -> Widget WN
colorBoth c = colorFG c . colorBG c

bold :: Widget WN -> Widget WN
bold = modifyDefAttr (`V.withStyle` V.bold)

italic :: Widget WN -> Widget WN
italic = modifyDefAttr (`V.withStyle` V.italic)

def :: Widget WN -> Widget WN
def = modifyDefAttr (const V.defAttr)

applyAttr :: V.Attr -> Widget WN -> Widget WN
applyAttr a = modifyDefAttr (const a)
