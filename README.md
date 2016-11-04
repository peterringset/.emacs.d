# .emacs.d

**My .emacs.d - use it, break it, fix it, trash it**

This emacs configuration is tested in emacs 25.1 on macOS. Some of the configurations aren't compatible with Windows, yet.

## Usage

Clone this into your home folder as `.emacs.d`. Rember to backup your old `.emacs.d` if you already have one:

```
git clone https://github.com/tmn/.emacs.d.git ~/.emacs.d
```

## OS dependencies

Install system dependencies for some of the emacs packages.

### macOS

```bash
brew install the_silver_searcher
```

### Linux (apt)

```bash
apt-get install silversearcher-ag
```

## Other dependencies

### elm-format

For elm-format to work you need to add elm-format to your system PATH. Download elm-format at [github.com/avh4/elm-format](https://github.com/avh4/elm-format)

### tern

Tern can be installed through npm on all platforms, or brew in macOS:

```bash
brew install homebrew/emacs/tern
```

For other installations methods. See [ternjs.net](ternjs.net)

Cheers!
