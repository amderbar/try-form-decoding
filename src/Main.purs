module Main where

import Prelude          (Unit, discard, ($))
import Effect           (Effect)
import Effect.Console   (log, logShow)
import Goat.Parser    as Parser
import Goat.Validator as Validator

main :: Effect Unit
main = do
  log "##### with Parser.toGoat"
  logShow $ Parser.toGoat { name: "", age: "", horns: "", contactType: "", email: "", phone: "", message: "" }
  logShow $ Parser.toGoat { name: "foo", age: "bar", horns: "baz", contactType: "ContactHoge", email: "", phone: "", message: "" }
  logShow $ Parser.toGoat { name: "foo", age: "-30", horns: "-1", contactType: "ContactEmail", email: "", phone: "", message: "message" }
  logShow $ Parser.toGoat { name: "foo", age: "30", horns: "3", contactType: "ContactPhone", email: "", phone: "", message: "message" }
  logShow $ Parser.toGoat { name: "foo", age: "30", horns: "2", contactType: "ContactEmail", email: "hoge@example.com", phone: "", message: "" }
  logShow $ Parser.toGoat { name: "foo", age: "30", horns: "2", contactType: "ContactPhone", email: "", phone: "0X0-ABCD-WXYZ", message: "message" }

  log "##### with Validator.toGoat"
  logShow $ Validator.toGoat { name: "", age: "", horns: "", contactType: "", email: "", phone: "", message: "" }
  logShow $ Validator.toGoat { name: "foo", age: "bar", horns: "baz", contactType: "ContactHoge", email: "", phone: "", message: "" }
  logShow $ Validator.toGoat { name: "foo", age: "-30", horns: "-1", contactType: "ContactEmail", email: "", phone: "", message: "message" }
  logShow $ Validator.toGoat { name: "foo", age: "30", horns: "3", contactType: "ContactPhone", email: "", phone: "", message: "message" }
  logShow $ Validator.toGoat { name: "foo", age: "30", horns: "2", contactType: "ContactEmail", email: "hoge@example.com", phone: "", message: "" }
  logShow $ Validator.toGoat { name: "foo", age: "30", horns: "2", contactType: "ContactPhone", email: "", phone: "0X0-ABCD-WXYZ", message: "message" }
