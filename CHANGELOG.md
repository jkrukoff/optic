# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.1.0] - 2019-04-28
### Added
- The `optic_array` module was added to work with the corresponding
  container.

## [3.0.0] - 2019-04-26
### Changed
- The argument order of `optic:map/3`, `optic:fold/4`, `optic:mapfold/4` and
  `optic:put/3` was changed to be the same as the corresponding lists module
  order; subject last.

## [2.1.0] - 2019-04-26
### Added
- The `optic_dict`, `optic_orddict`, `optic_gb_trees`, `optic_sets`,
  `optic_ordsets` and `optic_gb_sets` modules were added to work with the
  corresponding containers.

## [2.0.0] - 2019-02-24
### Added
- The `optic:wrap/2` and `optic:wrap/3` functions were added to enable
  modifying optics without exposing the internal representation.
- The `optic:chain/1` function was added to enable explicit optic composition.
- The `optic:is_optic/1` function was added.
- The `optic:create/3` optic wrapper was exposed as a public interface.
- The `optic:lax/1` optic wrapper was exposed as a public interface.
- The `optic:error/1` generic optic was added.
- The `optic:filter/1` generic optic was added.
- The `optic:require/1` generic optic was added.

### Changed
- The `optic:from/1` function was renamed to `optic:merge/1` to better
  distinguish it from chaining.
- The `optic_generic:id/0` optic was moved to `optic:id/0`.
- The internal `optic:'%extend'/3` interface was renamed to `optic:variations/3`.

## [1.0.0] - 2019-02-20
### Added
- Initial release.

[Unreleased]: https://github.com/jkrukoff/optic/compare/v3.1.0...HEAD
[3.1.0]: https://github.com/jkrukoff/optic/compare/v3.0.0...v3.1.0
[3.0.0]: https://github.com/jkrukoff/optic/compare/v2.1.0...v3.0.0
[2.1.0]: https://github.com/jkrukoff/optic/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/jkrukoff/optic/compare/v1.0.0...v2.0.0
[1.0.0]: https://github.com/jkrukoff/optic/releases/tag/v1.0.0
