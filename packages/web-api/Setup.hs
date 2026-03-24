import Core.Setup (copyLicenseFromRoot, coreMain)
import Core.Setup.PrerequisiteReport (reportSetupPrerequisites)

main :: IO ()
main = coreMain [copyLicenseFromRoot, reportSetupPrerequisites]
