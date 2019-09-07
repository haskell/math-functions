let
  pkgs = import <nixpkgs> {};
  pyp  = pkgs.python3.withPackages (py: with py;
    [ ipython
      mpmath
    ]);
in
pkgs.mkShell {
  buildInputs = [ pyp ];
}
