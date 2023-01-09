# Revision history for GHAppy

## 1.0.0.0 -- 2023-01-09

* Minor change triggering a major API change was introduced. Initially GHAppy
  would only retrieve Issues and PRs that were labeled with the `audit`
  label. This restriction has been lifted, which means that any previous report
  has to have this filter added to replicate the former functionality.
  I.e. files now have to be explicit about every label of an included Issue or
  PR.

## 0.2.0.0 -- 2022-12-12

* Introduced the YAML interface and working via the binary.

## 0.1.1.0 -- 2022-12-06

* Slightly changed the way linked-files are dealt with, together with adding new
  functions to the API.

## 0.1.0.0 -- 2022-12-06

* First version. Released on an unsuspecting world.
