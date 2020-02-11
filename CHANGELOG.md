# CHANGELOG

<!-- 
When editing this file, please include a link to the PR and/or issue for
the change.  
-->

## Unreleased

- Bump `base` lower bound to indicate GHC 7.10 as minimum supported version.
- Support Stack lts-13 (GHC 8.6) [#93](https://github.com/aloiscochard/codex/pull/93)
- Support Stack lts-14 (GHC 8.6.5) [#101](https://github.com/aloiscochard/codex/pull/101)
- Drop Stack lts-6 Support (GHC 7.10.3) [#101](https://github.com/aloiscochard/codex/pull/101)

## 0.5.2.0

This CHANGELOG entry is incomplete, as it is reconstructed from the Git history
since the previous release.

- Fix `codex update` inside of cabal sandbox [#74](https://github.com/aloiscochard/codex/pull/74)
- Look for project packages in sub-directories [#76](https://github.com/aloiscochard/codex/pull/76)
- Improved error message if Hackage path not found [#79](https://github.com/aloiscochard/codex/pull/79)
- Use `stack ls dependencies` to avoid deprecation notice [#81](https://github.com/aloiscochard/codex/pull/81)
- Only use Stack when `.stack-work` is present [#87](https://github.com/aloiscochard/codex/pull/87)
- Support Cabal >= 2.2 [#88](https://github.com/aloiscochard/codex/pull/88)
