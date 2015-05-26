# Codex

[![Join the chat at https://gitter.im/aloiscochard/codex](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/aloiscochard/codex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This tool download and cache the source code of packages in your local hackage,
it can then use this local cache to generate a `tags` file aggregating the sources of all the dependencies of a given cabal project.

You basically do `codex update` in your cabal project directory and you'll get a file (`codex.tags` by default, or `TAGS` when using
emacs format) that you can use in your favorite text editor.

*By default the generated tags file will include tags of the current project as well, this functionality can be disabled in your `~/.codex` file.*

## Install

Codex is published on [hackage](http://hackage.haskell.org/package/codex).

    cabal install codex

Note: You might have to install binary dependencies manually as cabal does not support them yet!

## Usage

By default `hasktags` will be used, and need to be in the `PATH`, the tagger command can be fully customized in `~/.codex`.

*The configuration file is automatically generated with default values when running the tool for the first time.*

    codex [update] [cache clean] [set tagger [hasktags|ctags]] [set format [vim|emacs|sublime]]

* **update**: Synchronize the tags file in the current cabal project directory (use --force to discard tags file hash)
* **cache clean**: Remove all `tags` file from the local hackage cache
* **set tagger [hasktags|ctags]**: Update the `~/.codex` configuration file for the given tagger
* **set format [vim|emacs|sublime]**: Update the `~/.codex` configuration file for the given format

*Note: codex will browse the parent directory for cabal projects and use them as dependency over hackage when possible.*

## VIM

Put this in your `.vimrc`:

    set tags=tags;/,codex.tags;/
    
**IMPORTANT**: You must use a version >= 7.4 (or you'll get `E431: Format error in tags file "codex.tags"`)

## FAQ

- I get `commitBuffer: invalid argument (invalid character)` when trying to use `codex` under Windows, is this a compatibility problem?

  It's an general issue with codec (http://jaspervdj.be/hakyll/tutorials/faq.html#hgetcontents-invalid-argument-or-commitbuffer-invalid-argument), which can be solved by running the following commands:

      $ /c/Windows/System32/chcp.com 65001
      $ codex cache clean
      $ rm codex.tags
      $ codex update
      
  (ref: https://github.com/aloiscochard/codex/issues/36)

