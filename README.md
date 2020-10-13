# dropdown-remote
Manipulate dropdown terminals in Emacs

This is a wrapper of D-Bus interface of yakuake and guake. Just view the code for the functions you need.

## Function

You are access yakuake and guake from a uniformed interface in Emacs through this package. There are the following functions for you:

### Main Functions

#### dropdown-toggle-window

  show or hide a dropdown terminal

#### dropdown-close-current-tab

  close current tab of dropdown terminal

#### dropdown-run-command-in-current-tab

  run command in current tab of dropdown terminal

### Tab Function

#### dropdown-current-tab

  get the id of current tab

#### dropdown-add-tab

  create a tab and get its id

#### dropdown-close-tab

  close a tab by its id

#### dropdown-run-command-in-tab

  run command in a specific tab

#### dropdown-set-tab-title

  rename the title of the specific tab



## Configuration

Yakuake and guake are both supported. Yakuake is in use by default. If you prefer guake, please set the variable `dropdown-terminal` to `'guake`.
