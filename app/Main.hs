module Main where

import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import qualified Text.Mustache as Mustache
import qualified Data.Yaml as Yaml
import qualified System.Environment as Env
import qualified System.FilePath as FP
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Mail.Mime as Mime
import qualified System.Process as Proc
import qualified System.Directory as Dir
import qualified Control.Monad.IO.Class as IO
import qualified Control.Exception as E
import qualified Paths_easy_invoice_maker

import qualified Invoice as Invoice
import qualified Email as Email

main :: IO ()
main = do
  invoiceFilePath <- head <$> Env.getArgs

  invoice <- prepareInvoice invoiceFilePath
  let invoiceHtml = FP.replaceExtension invoiceFilePath "html"
  templatesFilePath <- Paths_easy_invoice_maker.getDataFileName "templates"
  template <- automaticCompileThrow [templatesFilePath] "invoice.mustache"
  createHtml template invoice invoiceHtml

  let invoicePdf = FP.replaceExtension invoiceFilePath "pdf"
  createPdf invoiceHtml invoicePdf

  Dir.removeFile invoiceHtml

  sendEmail invoiceFilePath invoicePdf invoice

  where
    prepareInvoice :: FP.FilePath -> IO (Invoice.Invoice)
    prepareInvoice fp = do
      defaultParams :: Invoice.InvoiceDefaults <- Yaml.decodeFileThrow
        (FP.replaceFileName fp "defaults.yaml")
      bodyParams :: Invoice.InvoiceBody <- Yaml.decodeFileThrow fp
      let fromParams :: Invoice.From =
            case Invoice.bodyFrom bodyParams of
              Just x -> x
              Nothing -> case Invoice.defaultFrom defaultParams of
                Just x -> x
                Nothing -> Invoice.From "" Nothing ""
      currentDate <- date
      let invoiceNumber = T.pack $ FP.takeBaseName fp
      return $ Invoice.Invoice
        { Invoice.invNumber = invoiceNumber
        , Invoice.invDate = fromMaybe currentDate (Invoice.bodyDate bodyParams)
        , Invoice.invFrom = fromParams
        , Invoice.invTo = Invoice.bodyTo bodyParams
        , Invoice.invItems = Invoice.bodyItems bodyParams
        , Invoice.invBalanceDue = sum . map Invoice.itemTotalPrice $ Invoice.bodyItems bodyParams
        , Invoice.invNotes = Invoice.defaultNotes defaultParams
        }

    sendEmail :: FP.FilePath -> FP.FilePath -> Invoice.Invoice -> IO ()
    sendEmail invoiceFilePath invoicePdf invoice = do
      emailParams :: Email.Email <- Yaml.decodeFileThrow (FP.replaceFileName invoiceFilePath "email.yaml")
      let emailSubject = Email.subject emailParams
      let emailFrom = SMTP.Address
            (Just . Email.name . Email.from $ emailParams)
            (Email.address . Email.from $ emailParams)
      let emailTo = SMTP.Address
            (Just . Invoice.toName . Invoice.invTo $ invoice)
            (Invoice.toEmail . Invoice.invTo $ invoice)
      invoiceAttachment <- Mime.filePart "application/pdf" invoicePdf

      templatesFilePath <- Paths_easy_invoice_maker.getDataFileName "templates"
      emailTemplate <- automaticCompileThrow [templatesFilePath] "email.mustache"
      let emailTemplateParams = Email.Template
            { templateToName = Invoice.toName . Invoice.invTo $ invoice
            , templateInvoiceNumber = Invoice.invNumber invoice
            , templateFooter = Email.footer emailParams
            }
      let emailBody = Mime.plainPart
            . L.fromStrict
            . Mustache.substitute emailTemplate
            . Mustache.toMustache
            . Yaml.toJSON
            $ emailTemplateParams
      let parts = [emailBody, invoiceAttachment]
      let mail = SMTP.simpleMail
            emailFrom
            [emailTo]
            [] -- CC
            [] -- BCC
            emailSubject
            parts
      SMTP.sendMailWithLoginTLS
        (T.unpack . Email.server . Email.login $ emailParams)
        (T.unpack . Email.username . Email.login $ emailParams)
        (T.unpack . Email.password . Email.login $ emailParams)
        mail

-- | Get current date as format "May 8, 2022"
date :: IO T.Text
date = Time.getCurrentTime
  >>= return
  . T.pack
  . Time.formatTime Time.defaultTimeLocale "%b %-d, %Y"

-- | Apply mustache template to invoice, creating HTML file
createHtml :: Mustache.Template -> Invoice.Invoice -> FP.FilePath -> IO ()
createHtml template invoice fp = TIO.writeFile fp
  . Mustache.substitute template
  . Mustache.toMustache
  . Yaml.toJSON
  $ invoice

-- | Create PDF out of HTML using wkhtmltopdf
createPdf :: FP.FilePath -> FP.FilePath -> IO ()
createPdf inFile outFile = Proc.callProcess "wkhtmltopdf" [inFile, outFile]

newtype ParseException = ParseException String
  deriving (Show)
instance E.Exception ParseException
automaticCompileThrow :: [FP.FilePath] -> FP.FilePath -> IO (Mustache.Template)
automaticCompileThrow searchSpace f = IO.liftIO
  $ Mustache.automaticCompile searchSpace f >>= either (E.throwIO . ParseException . show) return
