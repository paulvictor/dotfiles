# Layout (dendritic pattern)

Every `.nix` file under `modules/` is a flake-parts module, loaded by `import-tree` from `flake.nix`.
Files and directories starting with `_` are skipped; they are plain files that a module refers to
(path modules, `callPackage` expressions, assets).

- `flake/` – flake plumbing: `flake.modules` option, the shared `pkgs`, and
  `features.nix` (the keyed feature modules, see below), `profiles.nix` (the `profiles` option), and
  `hosts.nix`, the only place that builds `nixosConfigurations` and `homeConfigurations`.
- `features/<name>/` – one concern, with everything for it in one place: the NixOS half
  (`flake.modules.nixos.<name>`), the home-manager half (`flake.modules.homeManager.<name>`),
  its overlay (`flake.overlays.<name>`), packages, and unfree allowances (`allowedUnfree`).
- `profiles/` – `profiles.<name> = { nixos = [ ... ]; home = [ ... ]; }`: flat named lists of features per role
  (`minimal`, `graphical`, `developer`, `laptop`, `work`, `workstation`). Profiles do not include other profiles.
- `hosts/<host>.nix` – `hosts.<host> = { profiles; nixos = {..}; home = {..}; }` plus hardware.
- `overlays/` – overlays that come from flake inputs.
- `me.nix` – the primary user's identity (`config.me`), shared by NixOS and home-manager; on NixOS it also creates that
  user's account. Root, sudo and the system-wide user policy are in `features/users`.

Features are referred to by attribute name, not by string: `config.features.<class>.<name>` (class is `nixos` or
`home`) is the feature module with a `key`. A profile or host writes
`with config.features.nixos; [ sway audio ]`, so a typo is an "undefined variable" error right there, and a name
that only exists for the other class fails the same way. Modules with the same key are imported once, so a feature
listed twice, or both directly and through a profile, is harmless. A host lists the `profiles` it wants explicitly
(`with config.profiles; [ minimal graphical ]`) and adds features of its own per class (`nixos.features`,
`home.features`). A profile cannot carry settings; put those in a feature.

Nothing is added implicitly: a lean host (cloud instance) lists just `minimal` (nix, `me`, `users`, ssh, basic tools) and
sets `home.enable = false`; `workstation` only holds my personal extras, so a workstation lists `minimal`, `graphical`, `developer` and `workstation`. Features that use
`config.me` need `me`, features that declare `sops.secrets` need `secrets`.

    hosts.my-vm.profiles = with config.profiles; [ minimal ];
    hosts.my-vm.stateVersion = "25.11";
    hosts.my-vm.nixos.modules = [ ./_my-vm-hardware.nix ];
    hosts.my-vm.home.enable = false;

Adding a feature: create `features/<name>/default.nix`, set `flake.modules.<class>.<name>`, and add
the attribute to a profile or a host. Per-host differences are options (e.g. `custom.sway.displayOffOnIdle`)
set in the host file, not hostname checks.
