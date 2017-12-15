# Trigger

`Trigger` is a cross platform file system watcher for super fast build-and-restart workflows. When files change, it can be configured to:
 - run build tasks
 - restart your app in the background

`fswatch` and others are good general purpose tools for triggering tasks on filesystem changes. 
However, they don't support stopping and starting background processes.

# How does it work?

```
         -------------------------------
        | start processes in background |
         -------------------------------
                       |
                 wait for change <-----------
                       |                     |
         -------------------------------     |
        |   stop background processes   |    |
         -------------------------------     |
                       |                     |
         -------------------------------     |
        | run build tasks sequentially  |    |
         -------------------------------     |
                       |                     |
         -------------------------------     |
        | start processes in background |    |
         -------------------------------     |
                       |                     |
                        ---------------------  

```

# Installation

It's Haskell, so download Stack.

Clone and run `stack build --install`

This should install `trigger` on your PATH.

If you get linking issues on Windows, see [this issue](https://github.com/commercialhaskell/stack/issues/425)

# Configuration

Create a file named `trigger.yaml`

Here is an example:
```yaml
- dirs:
    - "src"
  files:
    - "**/*.hs"
  ignore:
    - "**/Ignored.hs"
  tasks: 
    - "stack build"
  run:
    - command: "target/executable"
      workingDir: "target"
      env:
        - ["HOST", "localhost"]
        - ["PORT", "1234"]
```

Which consists of:

- `dirs`: one or more directories to watch for changes
- `files`: one or more file globs. Files that don't match will not trigger. 
- `ignore`: (optional) one or more file globs. Overrides the above file globs to exclude particular files. 
- `tasks`: (optional) one or more tasks, which are run sequentially in the foreground. Any error will stop subsequent tasks.
- `run`: (optional) one or more background processes. *Note: this executes the command directly, rather than using the shell, to improve shutdown behaviour.*
  - `command`: command and arguments to run in background. *Note: this command is relative to where you start trigger, not the `workingDir`.*
  - `workingDir`: (optional) override the working directory of the process.
  - `env`: (optional) additional environment variables for the process
