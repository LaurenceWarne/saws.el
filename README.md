# saws.el

## Installation

## Usage

The main entry point is `M-x` `saws`:

![saws-transient](https://github.com/user-attachments/assets/3bdcff76-fd89-4538-ac51-df71ac9efb59)

From this prefix command, the AWS region and profile can be set, and the following subcommands can be invoked:

### `l` -> `saws-logs-open-log-group`

Prompts the user for a log group and a relative time, and sticks output in a prettified buffer:

![saws-logs-open-log-group](https://github.com/LaurenceWarne/prefab.el/assets/17688577/53ea7508-65c5-4d0e-9d6c-1b84626118ed)

`?` can be used to view keybindings available in the log buffer.
