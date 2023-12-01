{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = {nixpkgs, ...}: let
    inherit (nixpkgs) lib;
    withSystem = f:
      lib.fold lib.recursiveUpdate {}
      (map f ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"]);
  in
    withSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) stdenv;
        appleDeps = with pkgs.darwin.apple_sdk.frameworks; [CoreServices];
      in {
        devShells.${system}.default =
          pkgs.mkShell
          {
            buildInputs = lib.optionals stdenv.isDarwin appleDeps;
            packages = with pkgs; [
              opam
              openssl
              zstd
            ];
            # LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            #   pkgs.stdenv.cc.cc
            # ];
          };
      }
    );
}
