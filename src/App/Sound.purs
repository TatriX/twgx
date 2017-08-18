module App.Sound where


import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Prelude (Unit)

foreign import data SOUND :: Effect

foreign import initSound :: forall eff. Eff (sound :: SOUND | eff) Unit
foreign import playTada :: forall eff. Aff (sound :: SOUND | eff) Unit
