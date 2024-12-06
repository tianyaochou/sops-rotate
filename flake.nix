{
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.devenv.url = "github:cachix/devenv";

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; }
    {
      imports = [ inputs.devenv.flakeModule ];
      systems = [ "x86_64-linux" "x86_64-darwin" ];
      perSystem = { pkgs, ... }:{
        devenv.shells.default = {
          packages = with pkgs; [
            haskellPackages.hoogle
            re2
          ];
          languages.haskell = {
            enable = true;
          };
        };
      };
    };
}
