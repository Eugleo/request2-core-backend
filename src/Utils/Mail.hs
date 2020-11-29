module Utils.Mail (
    sendmail',
    --sendmail,
    textMail',
    Mail (..),
    Address (..),
    Alternatives,
    htmlPart,
    plainPart,
    filePart,
    filePartBS,
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Network.Mail.Mime as M
import qualified Network.Mail.SMTP as S
import Server.Config


defaultFromAddr :: Config -> Address
defaultFromAddr c =
    Address
        { addressName = Just $ _mailFromName c,
          addressEmail = _mailFrom c
        }


sendmail' :: Config -> Mail -> IO ()
sendmail' c m =
    S.renderSendMailCustom
        "/usr/sbin/sendmail"
        ["-t", "-f", T.unpack (_mailEnvelopeFrom c)]
        m
            { M.mailFrom = defaultFromAddr c,
              mailHeaders = ("Reply-To", _mailReplyTo c) : mailHeaders m
            }


textMail' :: Config -> Address -> T.Text -> T.Text -> Mail
textMail' c to subj text = simpleMail' to (defaultFromAddr c) subj (L.fromStrict text)
