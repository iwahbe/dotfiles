# Dotfiles

Config files are stored in the `home` directory, laid out in the same way as they would be
in a users `$HOME` directory. Files and directories are
[softlinked](https://en.wikipedia.org/wiki/Symbolic_link) by `bin/config` from `./home` to
`$HOME`.

Secrets and personalization are handled by `git`, preventing the need to copy out of the
source tree and ensuring all users can have a clean `dotfiles` folder.

## Getting Started

The only tools that must be installed to work is `git`, `find`, `sed` and a POSIX shell
(`sh`). To get started, run `./bin/config init && ./bin/config up`.
