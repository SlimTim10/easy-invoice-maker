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

import qualified Invoice as Invoice
import qualified Email as Email

main :: IO ()
main = do
  invoiceFilePath <- head <$> Env.getArgs
  Mustache.automaticCompile ["templates"] "invoice.mustache" >>= \case
    Left err -> print err
    Right template -> do
      
      defaultParams :: Invoice.InvoiceDefaults <- Yaml.decodeFileThrow
        (FP.replaceFileName invoiceFilePath "defaults.yaml")
      bodyParams :: Invoice.InvoiceBody <- Yaml.decodeFileThrow invoiceFilePath
      let fromParams :: Invoice.From =
            case Invoice.bodyFrom bodyParams of
              Just x -> x
              Nothing -> case Invoice.defaultFrom defaultParams of
                Just x -> x
                Nothing -> Invoice.From "" Nothing ""
      currentDate <- date
      let invoiceNumber = T.pack $ FP.takeBaseName invoiceFilePath
      let invoice = Invoice.Invoice
            { Invoice.invNumber = invoiceNumber
            , Invoice.invDate = fromMaybe currentDate (Invoice.bodyDate bodyParams)
            , Invoice.invFrom = fromParams
            , Invoice.invTo = Invoice.bodyTo bodyParams
            , Invoice.invItems = Invoice.bodyItems bodyParams
            , Invoice.invBalanceDue = sum . map Invoice.itemTotalPrice $ Invoice.bodyItems bodyParams
            , Invoice.invNotes = Invoice.defaultNotes defaultParams
            }

      let invoiceHtml = FP.replaceExtension invoiceFilePath "html"
      createHtml invoice template invoiceHtml
      
      let invoicePdf = FP.replaceExtension invoiceFilePath "pdf"
      createPdf invoiceHtml invoicePdf
      
      Dir.removeFile invoiceHtml

      emailParams :: Email.Email <- Yaml.decodeFileThrow (FP.replaceFileName invoiceFilePath "email.yaml")
      let emailSubject = Email.subject emailParams
      let emailFrom = SMTP.Address
            (Just . Email.name . Email.from $ emailParams)
            (Email.address . Email.from $ emailParams)
      let emailTo = SMTP.Address
            (Just . Invoice.toName . Invoice.invTo $ invoice)
            (Invoice.toEmail . Invoice.invTo $ invoice)
      invoiceAttachment <- Mime.filePart "application/pdf" invoicePdf

      Mustache.automaticCompile ["templates"] "email.mustache" >>= \case
        Left err -> print err
        Right emailTemplate -> do
          let emailTemplateParams = Email.Template
                { Email.toName = Invoice.toName . Invoice.invTo $ invoice
                , Email.invoiceNumber = invoiceNumber
                , Email.footer = Email.footer (emailParams :: Email.Email)
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

date :: IO T.Text
date = Time.getCurrentTime
  >>= return
  . T.pack
  . Time.formatTime Time.defaultTimeLocale "%b %-d, %Y"

createHtml :: Invoice.Invoice -> Mustache.Template -> FP.FilePath -> IO ()
createHtml invoice template fp = TIO.writeFile fp
  . Mustache.substitute template
  . Mustache.toMustache
  . Yaml.toJSON
  $ invoice

-- | Create PDF out of HTML using wkhtmltopdf
createPdf :: FP.FilePath -> FP.FilePath -> IO ()
createPdf inFile outFile = Proc.callProcess "wkhtmltopdf" [inFile, outFile]
