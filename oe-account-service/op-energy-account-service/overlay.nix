{ GIT_COMMIT_HASH }: self: super: (import ./overlay-set.nix { GIT_COMMIT_HASH = GIT_COMMIT_HASH; } ) super
