import           Data.Either        (isRight)
import           Lib
import           System.IO
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.String

withFileSpec :: FilePath -> (String -> IO r) -> IO r
withFileSpec filename spec =
  withFile filename ReadMode $ \fd -> do
    contents <- hGetContents fd
    spec contents

expectedActiveJob :: ActiveJob
expectedActiveJob = ActiveJob
  { jobId = "4546817"
  , jobCondition = Nothing
  , userId = "axc1030"
  , activeState = Running
  , procs = 1
  , startTime = "Sat Dec 16 12:33:33"
}

testActiveJobStr = "4546817             axc1030    Running     1    -6:58:48  Sat Dec 16 12:33:33\n"

testJobSummary =
  "1148 active jobs      8096 of 19892 processors in use by local jobs (40.70%)\n" ++
  "                        528 of 818 nodes active      (64.55%)\n"

expectedJobSummary = JobSummary 8096 19892 528 818

testShowqOutput =
  "\n" ++
  "active jobs------------------------\n" ++
  "JOBID              USERNAME      STATE PROCS   REMAINING            STARTTIME\n" ++
  "\n" ++
  "4410289               gxk72    Running     1     109days  Thu Nov 30 20:40:38\n" ++
  "4410587               gxk72    Running     1     110days  Fri Dec  1 00:07:49\n" ++
  "4410588               gxk72    Running     1     110days  Fri Dec  1 00:11:08\n" ++
  "\n" ++
  "3126 active jobs      11992 of 20148 processors in use by local jobs (59.52%)\n" ++
  "                      784 of 828 nodes active      (94.69%)\n" ++
  "extra characters to be ignored"

main :: IO ()
main = hspec $
  describe "showq parser should" $ do
    it "parses basic job id" $
      runParser parseJobId () "test data" "4758010" `shouldBe` Right "4758010"

    it "parses extended job id with parens" $
      runParser parseJobId () "test data" "4758010(5)" `shouldBe` Right "4758010(5)"

    it "parses job condition" $
      runParser parseJobCondition () "test data" "*" `shouldBe` Right BackfilledAndPreemptible

    it "parses userids without numbers" $
      runParser parseUserId () "test data" "rac" `shouldBe` Right "rac"

    it "parses userids with numbers" $
      runParser parseUserId () "test data" "dvm105" `shouldBe` Right "dvm105"

    it "parses datetimes" $
      runParser parseDateTime () "test data" "Sat Dec 16 12:34:55" `shouldBe` Right "Sat Dec 16 12:34:55"

    it "parses active jobs" $
      runParser parseActiveJob () "test data" testActiveJobStr `shouldBe` Right expectedActiveJob

    it "parses the active jobs section of showq's output" $
      withFileSpec "test/testdata" $ \s ->
        length <$> runParser parseActiveJobSection () "test data" s `shouldBe` Right 1146

    it "parses the job summary" $
      runParser parseJobSummary () "test data" testJobSummary `shouldBe` Right expectedJobSummary

    it "parses sample showq output" $
      runParser showQParser ()  "test data" testShowqOutput `shouldSatisfy` isRight
