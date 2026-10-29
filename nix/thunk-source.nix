# Resolve a packed thunk or its unpacked checkout before selecting subdirectories.
path:
if builtins.pathExists (path + "/thunk.nix")
then import (path + "/thunk.nix")
else path
