let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs; 

pkgs.mkShell { 
  buildInputs = [ 
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-doc-preview
  ];
}