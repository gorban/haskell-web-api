import Distribution.Simple (defaultMainWithHooks)
import SetupHooks (setupHooks)

main :: IO ()
main = defaultMainWithHooks setupHooks
