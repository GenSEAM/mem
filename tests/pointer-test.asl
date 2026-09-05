(module asl-mem/pointer-test
  :d "Unit tests for perceptual pointers and blob offloading in asl-mem."
  :x [run-tests
      test-make-blob-pointer
      test-format-pointer-token
      test-extract-scalar-fact
      test-pointer-token-savings]
  :i [(pointer :a p)])

(df test-make-blob-pointer [] -> Bool
  :d "Verifies make-blob-pointer accurately computes byte size and token savings."
  (let [(raw "<html><body><h1>Hello World</h1><p>Sample text for offload test</p></body></html>")
        (ptr (p/make-blob-pointer "b3-dom-001" "dom" raw "DOM summary for hello page"))]
    (and (= (.-id ptr) "b3-dom-001")
         (and (= (.-kind ptr) "dom")
              (and (= (.-bytes ptr) (string-length raw))
                   (and (= (.-tokens-saved ptr) (/ (string-length raw) 4))
                        (= (.-summary ptr) "DOM summary for hello page")))))))

(df test-format-pointer-token [] -> Bool
  :d "Verifies formatting of BlobPointer into canonical S-expression token."
  (let [(ptr (p/BlobPointer
               :id "b3-123"
               :kind "dom"
               :bytes 16800
               :tokens-saved 4200
               :summary "Authentication form DOM tree"))
        (tok (p/format-pointer-token ptr))]
    (and (string-contains? tok "(:ptr :id \"b3-123\"")
         (and (string-contains? tok ":kind \"dom\"")
              (and (string-contains? tok ":tokens-saved 4200")
                   (string-contains? tok ":summary \"Authentication form DOM tree\")"))))))

(df test-extract-scalar-fact [] -> Bool
  :d "Verifies simulated sterile perception subagent fact extraction and formatting."
  (let [(ptr (p/make-blob-pointer "b3-dom-456" "dom" "<button id=\"sso\">Login SSO</button>" "SSO button"))
        (fact (p/extract-scalar-fact ptr "button_label" "Login SSO"))
        (fmt (p/format-perceptual-fact fact))]
    (and (= (.-key fact) "button_label")
         (and (= (.-val fact) "Login SSO")
              (and (> (.-confidence fact) 0.99)
                   (and (= (.-source-id fact) "b3-dom-456")
                        (string-contains? fmt "(:fact :key \"button_label\" :val \"Login SSO\"")))))))

(df test-pointer-token-savings [] -> Bool
  :d "Verifies token savings calculation for high-dimensional payloads."
  (let [(large-content "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
        (ptr (p/make-blob-pointer "b3-blob-big" "pdf" large-content "100-char PDF segment"))
        (tok (p/format-pointer-token ptr))]
    (and (= (.-bytes ptr) 100)
         (and (= (.-tokens-saved ptr) 25)
              (and (> (.-tokens-saved ptr) 0)
                   (< (string-length tok) (.-bytes ptr)))))))

(df run-tests [] -> Bool
  :d "Runs all perceptual pointer unit tests."
  (fold (fn [(acc Bool) (p Bool)] -> Bool (and acc p))
        true
        (list (test-make-blob-pointer)
              (test-format-pointer-token)
              (test-extract-scalar-fact)
              (test-pointer-token-savings))))

(df ! main [(args (List Str))] -> (Result Unit IoError)
  :d "Main entry point executing pointer unit tests."
  (if (run-tests)
    (let [(u (println "asl-mem pointer tests passed cleanly"))]
      (ok ()))
    (let [(u (eprintln "asl-mem pointer test failure"))]
      (err (other)))))
