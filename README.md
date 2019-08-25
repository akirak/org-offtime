[![Build Status](https://travis-ci.org/akirak/org-offtime.svg?branch=master)](https://travis-ci.org/akirak/org-offtime)

# org-offtime.el

org-offtime.el lets you track off time (break/nap/etc.) in the Emacs Org mode. "Starting an off-time clock" means running the following actions in sequence:

1. Start a clock on an entry in org-mode.
2. Run a suspend/lock command.

## Prerequisites

- Emacs 25.1 (with Org)
- [org-ql](https://github.com/alphapapa/org-ql)
- Ivy (optional)
- An external command used to suspend/lock your computer (e.g. slock)

## Installation

This package is not available on MELPA yet. Use quelpa or something.

## Configuration

See `offtime` and `org-offtime` customization groups.

[org-ql](https://github.com/alphapapa/org-ql) is used to define candidates selected using `org-offtime-clock-in` and `org-offtime-ivy`.

### Lock command

offtime package relies on an external command to suspend the computer or lock the screen. You have to configure at least one of the following variables:

- `offtime-suspend-command` is a shell command used to suspend the computer. Example: `systemctl suspend`.
- `offtime-lock-command` is a shell command used to lock the screen. Examples: `slock` and `physlock`.

You can invoke one of these actions without starting a clock by running `offtime-suspend` and `offtime-lock` command, respectively. `offtime-suspend` is the default action used by `org-offtime`. You can change this setting by customizing `org-offtime-default-action`. 

## Usage

- `org-offtime-clock-in` lets you select an entry to clock in.
- `org-offtime-clock-in-this-entry` starts a clock on the entry in Org mode.
- `org-offtime-ivy` is an Ivy version of `org-offtime-clock-in`. It supports alternative actions from `M-o`.

## License

GPL v3
