import Core.Setup (copyLicenseFromRoot, coreMain)

main :: IO ()
main = coreMain [copyLicenseFromRoot]
