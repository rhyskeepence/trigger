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
  exec:
    - "target/executable"      
```

Which consists of:

- `dirs`: one or more directories to watch for changes. To reduce file handles, it is best to keep this as scoped as possible (rather than using `.`).
- `files`: one or more file globs, relative to the current working directory. Files that don't match will not trigger. 
- `ignore`: (optional) one or more file globs. Overrides the above file globs to exclude particular files. 
- `clearScreen`: (optional boolean) The default is to clear the screen when a file changes. Set this to false to disable this behaviour.
- `tasks`: (optional) one or more tasks, which are run sequentially in the foreground. Any error will stop subsequent tasks.
- `exec`: (optional) one or more background processes.

This configuration can be repeated for other directories and tasks.