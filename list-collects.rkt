#lang racket/base

(provide get-top-level-collection-paths)

(require racket/list racket/path setup/link)

(define (add-directory-collections c s)
  (if (directory-exists? c)
      (for/fold ([s s]) ([p (in-list (directory-list c))]
                         #:when (directory-exists? (build-path c p))
                         #:when (regexp-match? #rx#"^[a-zA-Z_%+-]*$" p))
        (hash-set s (build-path c (path-element->string p)) #t))
      s))

(define (get-top-level-collection-paths)
  (hash-keys
   (for/fold ([s (hash)]) ([l (in-list
                               (current-library-collection-links))])
     (cond
      [(not l)
       (for/fold ([s s]) ([c (in-list
                              (current-library-collection-paths))])
         (add-directory-collections c s))]
      [(path? l)
       ;; TODO: this case generates false-positives for Zsh. For example, say
       ;; the package named PACK is linked at ~/code/PACK as a "top-level
       ;; collection" (list "PACK" "~/code/PACK"); then the below code will
       ;; typically result in ~/code ending up in Zsh's list of prefixes to
       ;; complete from. But ~/code probably contains more than just Racket
       ;; packages! In the worst case, a package's info.rkt might specify a
       ;; different name for the collection than either of PACK or ~/code/PACK
       ;; (in which case completion can't generate matches) so that the link is,
       ;; e.g., (list "COLLECT" "~/code/PACK"). Ideally we'd spit out a list of
       ;; collection names rather than generate a list of directory prefixes for
       ;; Zsh and then rely on Zsh traversing the filesystem.
       ;;
       ;; "Root" collections (list 'root <path>) don't appear to have this
       ;; problem. See "Collection Links"
       (let ([s (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #f #:with-path? #t))])
                  ;; TODO: see above. Here we throw away any root collection
                  ;; names. Because we don't go through
                  ;; add-directory-collections, either, we end up adding
                  ;; ~/code/PACK to the list of candidates, which gets turned
                  ;; into ~/code for Zsh.
                  (hash-set s (cdr c) #t))])
         (for*/fold ([s s]) ([c (in-list (links #:file l #:root? #t))])
           (add-directory-collections c s)))]
      [else (error 'get-top-level-collection-paths
                   "unexpected value in `current-library-collection-links': ~e"
                   l)]))))

(module+ main
  (define var "RACKET_COMPLETION_CLIENT")
  (define shell (getenv var))
  (cond [(equal? shell "bash")
         (for ([p (get-top-level-collection-paths)])
           (define-values [_base name _dir?] (split-path p))
           (displayln name))]
        [(equal? shell "zsh")
         (for-each displayln
                   (remove-duplicates
                    (map path-only (get-top-level-collection-paths))))]
        [else (error 'list-collects "unknown shell (missing ~a envvar)" var)]))
