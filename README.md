# org-offtime.el

org-offtime.el lets you track off-time (break/nap/etc.) in the Emacs Org-Mode. "Starting an off-time clock" means running the following actions in sequence:

1. Start a clock on an entry in org-mode.
2. Run a suspend/lock command.

## Prerequisites

- Emacs 25.1 (with Org)
- Ivy (optional)
- An external command used to suspend/lock your computer (e.g. slock)

## Installation

This package is not available on MELPA yet. Use quelpa or something.

## Configuration

You can configure org-offtime using the Emacs customize interface. You will probably have to configure the following items. For details, refer to `offtime` and `org-offtime` customize groups. 

### Org file

Customize `org-offtime-file` variable which is an org file to track off-time log. The default is offtime.org in `org-directory`. Headings in this file are listed in `org-offtime` and `counsel-org-offtime` commands. 

### Lock command

offtime package relies on an external command to suspend the computer or lock the screen. You have to configure at least one of the following variables:

- `offtime-suspend-command` is a shell command used to suspend the computer. Example: `systemctl suspend`.
- `offtime-lock-command` is a shell command used to lock the screen. Examples: `slock` and `physlock`.

You can invoke one of these actions without starting a clock by running `offtime-suspend` and `offtime-lock` command, respectively. `offtime-suspend` is the default action used by `org-offtime`. You can change this setting by customizing `org-offtime-default-action`. 

## Usage

You can start the off-time clock either from the file configured as `org-offtime-file`, or any given entry in org-mode:

- Pick a task from `org-offtime-file` using `org-offtime` or `counsel-org-offtime`. 

- To start an off-time clock on a given org entry under the cursor, use `org-offtime-clock-in`.

## License

GPL v3
