import           System.Environment (getArgs)
import           Test.DocTest       (mainFromCabal)

main :: IO ()
main = mainFromCabal "rzk" =<< getArgs
