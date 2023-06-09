(use-modules
 (guix packages)
 (guix git)
 (guix git-download)
 (guix build-system dune)
 (guix build-system ocaml)
 ((guix licenses) #:prefix license:)
 (gnu packages ocaml)
 (gnu packages libevent)
 (tarides packages ocaml))

(define-public ocaml-eio-scratch
  (package-with-ocaml5.0
   (package
    (name "ocaml-eio-scratch")
    (version "0.0.0")
    (home-page "https://github.com/adatario/eio-scratch")
    (source (git-checkout (url (dirname (current-filename)))))
    (build-system dune-build-system)
    (propagated-inputs
     (list ocaml5.0-eio
	   ocaml5.0-eio-main
	   libuv
	   ocaml-fmt))
    (native-inputs
     (list
      ;; dev tools
      ocaml-merlin
      ocaml-dot-merlin-reader))
    (synopsis #f)
    (description #f)
    (license license:isc))))

ocaml-eio-scratch
