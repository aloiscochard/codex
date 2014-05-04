# Codex

This tool download and cache the source code of packages in your local hackage,
it can then use this local cache to generate a `tags` file aggregating the sources of all the dependencies of a given cabal project.

You basically do `codex update` in your cabal project directory and you'll get a `codex.tags` file
that you can use in your favorite text editor.

## Install

Codex is published on [hackage](http://hackage.haskell.org/package/codex).

    cabal install codex

## Usage

By default `hasktags` will be used, and need to be in the `PATH`, the tagger command can be fully customized in `~/.codex`.

*The configuration file is automatically generated with default values when running the tool for the first time.*

    codex [update] [cache clean] [set tagger [hasktags|ctags]]

* **update**: Synchronize the `codex.tags` file in the current cabal project directory (use --force to discard tags file hash)
* **cache clean**: Remove all `tags` file from the local hackage cache
* **set tagger [hasktags|ctags]**: Update the `~/.codex` configuration file for the given tagger

*Note: codex will browse the parent directory for cabal projects and use them as dependency over hackage when possible.*

## VIM

Put this in your `.vimrc`:

    set tags=tags;/,codex.tags;/
    
**IMPORTANT**: You must use a version >= 7.4 (or you'll get `E431: Format error in tags file "codex.tags"`)

### Unlicense

This is free and unencumbered software released into the public domain.

#### Contribution Policy

*Contributions via GitHub pull requests are gladly accepted from their original author.
Along with any pull requests, please state that the contribution is your original work and 
that you license the work to the project under the project's open source license.
Whether or not you state this explicitly, by submitting any copyrighted material via pull request, 
email, or other means you agree to license the material under the project's unlicense and 
warrant that you have the legal authority to do so.*
