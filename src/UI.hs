module UI where
import Ipair 

data WidgetName = Tilename Ipair | Background deriving (Eq, Ord, Show)
type WN = WidgetName