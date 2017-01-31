Tools needed:
  Both:
  - Make (while not explicitly needed it will make developing on this
    library way easier)

  Haskell:
  - cabal or stack
  - GHC 8.0.1 or later
  - haddock (for documentation only)

  Rust:
  - cargo
  - rustc

Steps to introduce a change to the codebase:

1. Fork the Repo
2. Make a new branch off of master
3. Implement your code change
4. Make sure it works
5. Rebase off of the latest version of master
6. Open a PR
7. Title must be 50 characters or less and in the imperative mood
8. Describe why you've made the change for this PR and what it does and
   why you did it.
9. If changes are asked for by whoever is assigned to your PR then make
   them all and do not force push whatsoever. They will all be squashed
   and merged after reviewing each change.

Commits:

A good commit log is imperative. Here are the requirements your commits
must have to be accepted.

1. Commit message is 50 characters or less and in the imperative mood.
   This describes the change that will occur if this commit is used. If
   you're finding it hard to summarize in 50 characters split the commit
   up.
2. The body of the commit message will describe why the change was made.
   This is optional if the change is self evident such as "Add missing
   file needed for Rust tests".
