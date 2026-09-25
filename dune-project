(lang dune 3.17)

(version 0.1.0)

(name caravan)

(generate_opam_files true)

(source
 (github naamanu/caravan))

(license MIT)

(authors "Nana Kwadwo <nkwadwo06@gmail.com>")

(maintainers "Nana Kwadwo <nkwadwo06@gmail.com>")

(documentation "https://naamanu.github.io/caravan")

(package
 (name caravan)
 (synopsis "Durable, distributed background jobs for OCaml 5 and Eio")
 (description
  "Caravan is a durable job queue: producers enqueue jobs to shared storage and workers on many nodes claim and execute them with retries, scheduling, priorities, uniqueness, cron and graceful shutdown. Delivery is at-least-once.")
 (depends
  (ocaml
   (>= 5.2))
  (eio
   (>= 1.2))
  (yojson
   (>= 2.1))
  (ptime
   (>= 1.1))
  (mtime
   (>= 2.0))
  (logs
   (>= 0.7))
  (fmt
   (>= 0.9))
  (eio_main :with-test)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (ppx_deriving_yojson :with-test)
  (odoc :with-doc)))

(package
 (name caravan-postgres)
 (synopsis "PostgreSQL backend for Caravan")
 (description
  "Stores Caravan jobs in PostgreSQL, claims work with FOR UPDATE SKIP LOCKED, wakes workers with LISTEN/NOTIFY and supports transactional enqueue. Talks to libpq directly, driven non-blocking by Eio.")
 (depends
  (caravan
   (= :version))
  (eio
   (>= 1.2))
  (postgresql
   (>= 5.0))
  (yojson
   (>= 2.1))
  (ptime
   (>= 1.1))
  (logs
   (>= 0.7))
  (eio_main :with-test)
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name caravan-web)
 (synopsis "Web dashboard, JSON API and Prometheus metrics for Caravan")
 (depends
  (caravan
   (= :version))
  (cohttp-eio
   (>= 6.0))
  (http
   (>= 6.0))
  (base64
   (>= 3.0))
  (uri
   (>= 4.0))
  (yojson
   (>= 2.1))
  (ptime
   (>= 1.1))
  (logs
   (>= 0.7))
  (eio_main :with-test)
  (alcotest :with-test)))

(package
 (name caravan-cli)
 (synopsis "Command-line tool for operating Caravan queues")
 (depends
  (caravan
   (= :version))
  (caravan-postgres
   (= :version))
  (caravan-web
   (= :version))
  (cmdliner
   (>= 1.2))
  (eio_main
   (>= 1.2))
  (fmt
   (>= 0.9))
  (logs
   (>= 0.7))))
