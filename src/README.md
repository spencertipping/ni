# Root directory
Places you can safely modify, within reason:

- `home/`: a place where you can stash small-ish files and change configuration
  settings
- `extensions/`: where you can install third-party addons for ni

**Any file you create ending in `.sh` will be executed when ni starts, which is
most likely not what you want to do.** To avoid this, you can `require` the
file from `home/conf`, which is loaded after all ni files and extensions.
