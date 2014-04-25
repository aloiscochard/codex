# Codex

This tool download and cache the source code of packages in your local hackage,
he can then use this local cache to generate a `tags` file aggregating the sources of all the dependencies of a given cabal project.

You basically do `codex update` in your cabal project directory and you'll get a `codex.tags` file
that you can use in your favorite text editor.

## Exuberant Ctags

Here is the a minimal haskell configuration for ctags (`~/.ctags`)

    --exclude='.git'
    --exclude='*dist*'
    --exclude='*.cabal-sandbox*'

    --langdef=haskell
    --langmap=haskell:.hs
    --regex-haskell=/^module[ \s]([a-zA-Z0-9]*)[ \s]where/\1/m,module/
    --regex-haskell=/^class[ \s](.*)where/\1/t,typeclasses/
    --regex-haskell=/^instance[ \s](.*)where/\1/c,instances/
    --regex-haskell=/^data[ \s]([a-zA-Z0-9]*)[ \s]/\1/d,data/
    --regex-haskell=/^newtype[ \s]([a-zA-Z0-9]*)[ \s]/\1/n,newtype/
    --regex-haskell=/^type[ \s]([a-zA-Z0-9]*)[ \s]/\1/t,type/
    --regex-haskell=/^([ \sa-zA-Z0-9]*)[ \s]::[ \s](.*)$/\1/f,functions/

### Unlicense

This is free and unencumbered software released into the public domain.

#### Contribution Policy

*Contributions via GitHub pull requests are gladly accepted from their original author.
Along with any pull requests, please state that the contribution is your original work and 
that you license the work to the project under the project's open source license.
Whether or not you state this explicitly, by submitting any copyrighted material via pull request, 
email, or other means you agree to license the material under the project's unlicense and 
warrant that you have the legal authority to do so.*
