(package chez-test)
(version "0.0.1")

(dependencies
    (chez ">=9.5"))

(libdirs "src")

(executables
  (main "src/chez-test.scm"))

(commands
    (test (depends build) "./top-level/chez-test test"))
