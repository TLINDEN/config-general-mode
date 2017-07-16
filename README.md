# config-general-mode - Emacs major editing mode for Config::General config files.

[![MELPA](http://melpa.org/packages/config-general-mode-badge.svg)](http://melpa.org/#/config-general-mode)

## Screenshot

![demo](https://raw.githubusercontent.com/TLINDEN/config-general-mode/master/config-general-screenshot.png)


## Introduction

[Config::General](http://search.cpan.org/dist/Config-General/) is a
Perl   module  for   parsing  config   files  with   some  enhanced
features.  `config-general-mode` makes it easier to edit such config
files with Emacs.

It is based on `conf-mode` with the following features:

- good syntax highlighting for config files
- completion support with <kbd>TAB</kbd> (using `dabbrev`)
- imenu support for <blocks>
- electric paring mode (for quotes, parens, etc) enabled
- automatic indenting
- jump to include file with <kbd>C-return</kbd>

## Installation

To use, save config-general-mode.el to a directory in your load-path.

Add something like this to your config:

        (require 'config-general-mode)
        (add-to-list 'auto-mode-alist '("\\.conf$" . config-general-mode))

or load it manually, when needed:

    M-x config-general-mode

You can also enable it with  a buffer-local variable by adding this as
the first line of a config file:

    # -*-config-general-*-

## Usage

Edit  your config  file as  usual.  Use  <kbd>TAB</kbd> for  completion of
values and variables.  Use <kbd>C-c C-t</kbd>  to toggle flags (like true to
false). Use <kbd>C-c C-=</kbd> on a region to automatically align on the `=`
character.  Use <kbd>C-c  C-/</kbd> to breakup a region with  long lines into
shorter ones  using backslash notation.  Use  <kbd>C-return</kbd> to visit
an included file  or (when not on  a link) insert a  new line below
the current one, indent and move point there.  Use <kbd>C-k</kbd> to delete
lines, including continuation lines or  whole blocks.  Use <kbd>C-c C-j</kbd>
to  jump to  a block  definition (same  as using  `imenu` with  the
mouse).

## Customize

You can customize the mode with:

     M-x customize-group RET config-general-mode RET

You can also use hooks to config-general  mode as a way to modify or enhance
its behavior.  The following hooks are available:

    config-general-mode-hook

For example:

        (add-hook 'config-general-mode-hook 'electric-indent-mode)

## Meta


Copyright (C) 2017, T.v.Dein <tlinden AT cpan DOT org>

This file is NOT part of Emacs.

This  program is  free  software; you  can  redistribute it  and/or
modify it  under the  terms of  the GNU  General Public  License as
published by the Free Software  Foundation; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
General Public License for more details.

You should have  received a copy of the GNU  General Public License
along  with  this program;  if  not,  write  to the  Free  Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA

    - Version: 0.01
    - Author: T.v.Dein <tlinden AT cpan DOT org>
    - Keywords: config file editing
    - URL: https://github.com/tlinden/config-general-mode
    - License: GNU General Public License >= 2
