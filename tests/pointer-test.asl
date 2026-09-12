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
    (assert (= (.-id ptr) "b3-dom-001") "id matches")
    (assert (= (.-kind ptr) "dom") "kind matches")
    (assert (= (.-bytes ptr) (string-length raw)) "bytes match")
    (assert (= (.-tokens-saved ptr) (div-i64 (string-length raw) 4)) "tokens saved match")
    (assert (= (.-summary ptr) "DOM summary for hello page") "summary matches")
    true))

(df test-format-pointer-token [] -> Bool
  :d "Verifies formatting of BlobPointer into canonical S-expression token."
  (let [(ptr (p/BlobPointer
               :id "b3-123"
               :kind "dom"
               :bytes 16800
               :tokens-saved 4200
               :summary "Authentication form DOM tree"))
        (tok (p/format-pointer-token ptr))]
    (assert (string-contains? tok "(:ptr :id \"b3-123\"") "token contains id")
    (assert (string-contains? tok ":kind \"dom\"") "token contains kind")
    (assert (string-contains? tok ":tokens-saved 4200") "token contains tokens-saved")
    (assert (string-contains? tok ":summary \"Authentication form DOM tree\")") "token contains summary")
    true))

(df test-extract-scalar-fact [] -> Bool
  :d "Verifies simulated sterile perception subagent fact extraction and formatting."
  (let [(ptr (p/make-blob-pointer "b3-dom-456" "dom" "<button id=\"sso\">Login SSO</button>" "SSO button"))
        (fact (p/extract-scalar-fact ptr "button_label" "Login SSO"))
        (fmt (p/format-perceptual-fact fact))]
    (assert (= (.-key fact) "button_label") "key matches")
    (assert (= (.-val fact) "Login SSO") "val matches")
    (assert (> (.-confidence fact) 0.99) "confidence > 0.99")
    (assert (= (.-source-id fact) "b3-dom-456") "source-id matches")
    (assert (string-contains? fmt "(:fact :key \"button_label\" :val \"Login SSO\"") "formatted fact matches")
    true))

(df test-pointer-token-savings [] -> Bool
  :d "Verifies token savings calculation for high-dimensional payloads."
  (let [(large-content "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789")
        (ptr (p/make-blob-pointer "b3-blob-big" "pdf" large-content "100-char PDF segment"))
        (tok (p/format-pointer-token ptr))]
    (assert (= (.-bytes ptr) 100) "bytes 100")
    (assert (= (.-tokens-saved ptr) 25) "tokens saved 25")
    (assert (> (.-tokens-saved ptr) 0) "tokens saved > 0")
    (assert (< (string-length tok) (.-bytes ptr)) "token string smaller than bytes")
    true))

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
