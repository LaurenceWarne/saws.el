# saws.el

`saws.el` is a WIP glorified convenience wrapper around the [AWS CLI v2](https://awscli.amazonaws.com/v2/documentation/api/latest/index.html).

## Installation

Currently installing from source is the only option:

```elisp
(use-package saws
  :ensure nil
  :quelpa (saws :fetcher github :repo "LaurenceWarne/saws" :upgrade t)
  :commands saws)
```


## Usage

`saws-region` and `saws-profile` can be used to configure the region and profile used by `saws.el` commands (e.g. set in `.dir-locals.el` etc).  These can also be set from the `saws` command below (see below), which has the additional effect of setting the variable for the current buffer (this is helpful for history).

The main entry point is `M-x` `saws`:

![saws-transient](https://github.com/user-attachments/assets/b5542943-abb5-4755-93ed-de4c31d477fc)

From this prefix command, the AWS region and profile can be set, and the following subcommands can be invoked:

### `l` -> `saws-logs-open-log-group`

Prompts the user for a log group and a relative time, and sticks output in an auto-updating prettified buffer (A wrapper around `aws logs tail`):

![saws-logs-open-log-group](https://github.com/LaurenceWarne/prefab.el/assets/17688577/53ea7508-65c5-4d0e-9d6c-1b84626118ed)

`?` can be used to view keybindings available in the log buffer.

### `#` -> `saws-secrets`

Allows the user to copy the value of a prompted for secret to the kill ring.
