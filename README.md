# hnix-store-experiments
======================

Dragons and unsorted stuff, currently mainly
utilities for querying binary caches.

## Executables

### `hnix-store-in-cache`

Can be used to filter paths from `stdin` which
are not availabe on [cache.nixos.org](https://cache.nixos.org)

This is useful in combination with [cachix](https://cachix.org)
to only push cache paths not available in official cache.

```bash
nix-shell --command 'exit'
nix-store -qR --include-outputs $(nix-instantiate shell.nix) \
  | hnix-store-in-cache | cachix push ivory-tower-nix
```
