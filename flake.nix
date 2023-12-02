{
	inputs = {
		nixpkgs.url     = "github:nixos/nixpkgs/nixpkgs-unstable";
		flake-utils.url = "github:numtide/flake-utils";
	};

	outputs = { self, nixpkgs, flake-utils }:
		flake-utils.lib.eachDefaultSystem(system:
			let
				pkgs = nixpkgs.legacyPackages.${system};
				lib = nixpkgs.lib;
				custom-haskell = pkgs.ghc.withPackages(p: with p; [
					split
					haskell-language-server
				]);
				cppfront =
					pkgs.stdenv.mkDerivation rec {
						pname = "cppfront";
						version = "20231124";

						src = pkgs.fetchFromGitHub {
							owner = "hsutter";
							repo = "cppfront";
							rev = "4bd0c0438f2d3fa65d3e65a55b17c3a296bd8bc3";
							sha256 = "sha256-SNMukVZ0yPach9XX3Sc37a7KrQRkX+K0KA7pLAIChoc=";
						};

						nativeBuildInputs = [ pkgs.stdenv ];
						buildInputs = [ pkgs.gcc ];

						buildPhase = ''
							g++ ${src}/source/cppfront.cpp -std=c++20 -O3 -march=alderlake -o cppfront
						'';

						installPhase = ''
							mkdir -p $out/bin
							ls $out
							cp cppfront $out/bin
							cp -r $src/include $out
							echo "c++ -isystem $out/include -std=c++20 \$@" > $out/bin/c++2
							echo "echo $out/include" > $out/bin/cpp2util-path
							chmod +x $out/bin/c++2
							chmod +x $out/bin/cpp2util-path
						'';

						meta = with lib; {
							description = "A personal experimental C++ Syntax 2 -> Syntax 1 compiler";
							homepage = "https://github.com/hsutter/cppfront";
							# license = licenses.CC-BY-NC-ND-4.0;
							platforms = platforms.linux;
						};
					};
			in rec {
				devShells.default = pkgs.mkShell {
					nativeBuildInputs = with pkgs; [
						custom-haskell
						haskellPackages.stylish-haskell
						haskellPackages.hindent

						rustc
						cargo
						clippy
						rustfmt
						rust-analyzer

						hyperfine
						
						pkgs.gnumake
						stdenv
						cppfront
					];
				};
			}
		);
}
