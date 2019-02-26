# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
