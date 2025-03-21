module Adapter.Email.SendMail where



import ClassyPrelude
import System.Process (spawnProcess, waitForProcess)
import System.IO (hPutStr, hClose)
import qualified Domain.Auth as D
import qualified Data.Text.IO as TIO

type Recipient = D.Email
type Subject = Text
type Body = Text


sendMail :: MonadIO m => Recipient -> Subject -> Body -> m ()
sendMail email subject body =
  liftIO $ TIO.putStrLn $ "Send mail mock:" <> "\n\tRecipient: " <> D.emailRaw email <> "\n\tSubject: " <> subject <> "\n\tBody: " <> body

-- import System.IO.Temp (withSystemTempFile)

-- sendEmail :: String -> String -> String -> IO ()
-- sendEmail recipient subject body = do
--     withSystemTempFile "email.txt" $ \filePath handle -> do
--         -- Write the email content to a temporary file
--         hPutStr handle $ unlines
--             [ "To: " ++ recipient
--             , "Subject: " ++ subject
--             , "Content-Type: text/plain; charset=UTF-8"
--             , ""
--             , body
--             ]
--         hClose handle

--         -- Send the email using sendmail
--         process <- spawnProcess "sendmail" ["-t"]  -- `-t` makes sendmail read headers from input
--         waitForProcess process

--         putStrLn "Email sent successfully!"
