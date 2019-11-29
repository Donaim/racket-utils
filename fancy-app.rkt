;; This module is a modified version of original "compose-app" by jackfirth that was published at https://github.com/jackfirth/compose-app under Apache 2.0 license.

#lang racket/base

(provide (rename-out [compose-app/fancy-app #%app])
         compose-app/fancy-app
         (all-from-out "compose-app.rkt"))

(require (for-syntax racket/base)
         (rename-in "fancy-app-source.rkt" [#%app fancy-app])
         (except-in "compose-app.rkt" #%app)
         racket/stxparam
         syntax/parse/define)

(define-simple-macro (compose-app/fancy-app any ...)
  (syntax-parameterize
      ([compose-app-base-app (make-rename-transformer #'fancy-app)])
    (compose-app any ...)))
