## v0.2.0

### Features
- Add Haskell Runtime Support to Rust Library
- Change build.rs script to pull in the base libraries
  needed to get the Runtime Working.
- Change structuring of Haskell code so import statements
  for modules is:
  ```haskell
  import Curryrs.Types
  ```
  and not:
  ```haskell
  import Types
  ```

### Documentation
- Update README with how to properly call Haskell code in
  Rust with the new library updates by calling the Runtime
  from within Rust. It also now makes setting up the Haskell
  code a lot easier removing the need for header files and
  using gcc to get code to work.

### Testing
- Setup Haskell in Rust tests with C glue code to get it to
  work

## v0.1.1

### Features
- Change FFI Primitives to use fixed width versions in Haskell
  and Rust rather than using C types.

### Documentation
- Update README to make documentation clearer to users based
  off feedback in issue #1
- Update README with how to get Haskell into Rust

### Testing
- Setup Haskell in Rust tests with C glue code to get it to
  work

## v0.1.0

### Features
- Implement FFI Primitives for Rust and Haskell

### Testing
- Add tests for Rust in Haskell

### Documentation
- Add README.md file and have it reflect the state of the project
- Add CONTRIBUTING.md file
- Document Haskell and Rust code to have 100% coverage

### Misc
- Add MIT/APACHE-2.0 license files
