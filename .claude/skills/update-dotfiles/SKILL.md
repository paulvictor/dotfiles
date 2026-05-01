---
name: update-dotfiles
description: Update flake inputs in ~/dotfiles/nix and verify all NixOS and home-manager configurations build successfully
---

Update flake inputs and verify all NixOS/home-manager configs still build.

Arguments: $ARGUMENTS (optional: a specific input name to update, e.g. "nixpkgs" or "homeManager". If empty, update all inputs.)

Flake root: ~/dotfiles/nix
Current inputs and last-modified dates: !`nix flake metadata ~/dotfiles/nix 2>&1 | tail -40`

## Procedure

Work from ~/dotfiles/nix for all commands.

### 1. Update inputs

- If $ARGUMENTS is non-empty: `nix flake update $ARGUMENTS --flake ~/dotfiles/nix`
- Otherwise: `nix flake update ~/dotfiles/nix`

Then show a summary of what changed:
```
git -C ~/dotfiles diff nix/flake.lock
```

If nothing changed in flake.lock, report that and stop — no rebuild needed.

### 2. Build NixOS configurations

Build each host's toplevel with `--no-link` (pulls from cache where possible):

```bash
nix build ~/dotfiles/nix#nixosConfigurations.anarki.config.system.build.toplevel --no-link
nix build ~/dotfiles/nix#nixosConfigurations.bones.config.system.build.toplevel --no-link
nix build ~/dotfiles/nix#nixosConfigurations.sarge.config.system.build.toplevel --no-link
nix build ~/dotfiles/nix#nixosConfigurations.sorlag.config.system.build.toplevel --no-link
nix build ~/dotfiles/nix#nixosConfigurations.uriel.config.system.build.toplevel --no-link
nix build ~/dotfiles/nix#nixosConfigurations.orbb.config.system.build.toplevel --no-link
```

Run these sequentially so errors are easy to attribute to a specific host.

### 3. Build home-manager configurations

```bash
nix build '~/dotfiles/nix#legacyPackages.x86_64-linux.homeConfigurations."viktor@sarge".activationPackage' --no-link
nix build '~/dotfiles/nix#legacyPackages.x86_64-linux.homeConfigurations."viktor@anarki".activationPackage' --no-link
nix build '~/dotfiles/nix#legacyPackages.x86_64-linux.homeConfigurations."viktor@uriel".activationPackage' --no-link
nix build '~/dotfiles/nix#legacyPackages.x86_64-linux.homeConfigurations."viktor@sorlag".activationPackage' --no-link
nix build '~/dotfiles/nix#legacyPackages.x86_64-linux.homeConfigurations."viktor@bones".activationPackage' --no-link
```

Skip `paul.victor@crash` — it's a macOS/nix-darwin config, not buildable on Linux.

### 4. Report and offer to commit

Print a table: each config (NixOS + home), pass/fail status, and any error summary.

If all passed and flake.lock changed, offer to commit:
```
git -C ~/dotfiles add nix/flake.lock
git -C ~/dotfiles commit -m "flake: update inputs (<list changed inputs>)"
```

If any failed:
- Show the full error for each failure
- Suggest likely causes: removed NixOS option, renamed package, API break in the updated input
- Do NOT commit flake.lock if any config fails
