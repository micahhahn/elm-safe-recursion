# Changelog

## [2.0.0] - 2022-09-01
### Changed
- Completely gutted the underlying CPS implementation in favor of something much simpler and faster
- The API has changed substantially:
    - We no longer provide left and right folds as we must always fold from the left anyways.  The user can reverse the input if that behavior is required. 
    - Dictionary folds now also have access to the key.
    - Many continuation accepting functions have been added (in the form of `___Then`) for performance.
    - New type variable letters have been chosen that should be more intuitive.

## [1.0.1] - 2022-07-28

- Drastically improved documentation

## [1.0.0] - 2022-07-25

- This is the first release of `elm-safe-recursion`.

