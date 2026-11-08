{
  description = "Jolt, a Clojure implementation on Chez Scheme";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    makes = {
      url = "github:makeplus/makes/d14cb578c2f04e6f9d00e01c7c4e416a9baf94e9";
      flake = false;
    };
    self.submodules = true;
  };

  outputs =
    {
      self,
      nixpkgs,
      makes,
    }:
    let
      # Intel macOS is deferred: releases cross-build x86_64-macos from the
      # arm64 runner, and Nix has no equivalent cross path here to validate.
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      # nixpkgs' Chez with jolt's kernel patches (host/chez/chez-patches, in
      # name order, as ci/apply-chez-patches.sh applies them for the release
      # builds). Without them a Nix-built jolt keeps stock Chez's placement of
      # foreign-call arguments past the registers, which upstream has fixed
      # but not released, and pays the stock price for collect-safe calls.
      # The patches are cut against v10.4.1, the version nixpkgs packages.
      patchedChez =
        pkgs:
        pkgs.chez.overrideAttrs (old: {
          patches =
            (old.patches or [ ])
            ++ map (f: ./host/chez/chez-patches + "/${f}") (
              builtins.sort builtins.lessThan (
                builtins.filter (f: pkgs.lib.hasSuffix ".patch" f) (
                  builtins.attrNames (builtins.readDir ./host/chez/chez-patches)
                )
              )
            );
        });
      mkJolt =
        pkgs:
        let
          chez = patchedChez pkgs;
          version = self.shortRev or "dev";
          runtimePath = pkgs.lib.makeBinPath [
            pkgs.git
          ];
          opensslLibraryPath = pkgs.lib.makeLibraryPath [ pkgs.openssl ];
          cacertFile = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
        in
        pkgs.stdenv.mkDerivation {
          pname = "jolt";
          inherit version;
          src = self;

          strictDeps = true;
          nativeBuildInputs = [
            chez
            pkgs.makeWrapper
            pkgs.pkg-config
            pkgs.xxd
          ];
          buildInputs = [
            pkgs.lz4
            pkgs.zlib
            pkgs.ncurses
            pkgs.openssl
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.libuuid ]
          ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.libiconv ];

          # Flake sources contain no .git directory, so build-jolt.ss cannot
          # derive the version with git describe.
          JOLT_VERSION = version;

          dontConfigure = true;

          buildPhase = ''
            runHook preBuild
            scheme --script host/chez/build-jolt.ss release target/release/jolt
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p "$out/bin"
            install -m755 target/release/jolt "$out/bin/jolt"
            mkdir -p "$out/share/doc/jolt"
            cp LICENSE NOTICE "$out/share/doc/jolt/"
            cp -R licenses "$out/share/doc/jolt/licenses"
            runHook postInstall
          '';

          # jolt.deps invokes git (jars extract in process); jolt.mvn-http
          # dlopens OpenSSL at fetch time through the JOLT_OPENSSL_LIBDIR seam
          # (its macOS built-in candidates are Homebrew paths, so no loader-path
          # variable could cover Darwin). Pin both to the Nix package closure.
          postFixup = ''
            wrapProgram "$out/bin/jolt" \
              --prefix PATH : "${runtimePath}" \
              --set-default JOLT_OPENSSL_LIBDIR "${opensslLibraryPath}" \
              --set-default SSL_CERT_FILE "${cacertFile}"
          '';

          meta = {
            description = "Clojure implementation on Chez Scheme";
            homepage = "https://jolt-lang.net";
            # jolt itself, plus what NOTICE lists as embedded in the binary
            license = with pkgs.lib.licenses; [
              epl20
              epl10
              asl20
              mit
              bsd2
              bsd3
              zlib
            ];
            mainProgram = "jolt";
          };
        };
    in
    {
      packages = forAllSystems (
        pkgs:
        let
          jolt = mkJolt pkgs;
        in
        {
          inherit jolt;
          default = jolt;
        }
      );

      apps = forAllSystems (pkgs: {
        default = {
          type = "app";
          program = "${self.packages.${pkgs.stdenv.hostPlatform.system}.jolt}/bin/jolt";
          meta.description = "Run Jolt";
        };
      });

      devShells = forAllSystems (
        pkgs:
        let
          chez = patchedChez pkgs;
        in
        {
          default = pkgs.mkShell {
            packages = [
              # Required by makeplus/makes during Makefile initialization.
              pkgs.bash
              pkgs.which
              chez
              pkgs.stdenv.cc
              pkgs.gnumake
              pkgs.pkg-config
              pkgs.xxd

              # Jolt deps.edn support
              pkgs.git
              pkgs.openssl

              # Chez/Jolt native-link dependencies
              pkgs.lz4
              pkgs.zlib
              pkgs.ncurses
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.libuuid ]
            ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.libiconv ];

            # Make uses this locked source-only input instead of cloning makes.
            M = makes;
            JOLT_CHEZ = "${chez}/bin/scheme";
            CHEZ = "${chez}/bin/scheme";
            JOLT_OPENSSL_LIBDIR = pkgs.lib.makeLibraryPath [ pkgs.openssl ];

            shellHook = ''
              echo "Jolt development environment"
              printf 'Chez: '
              printf '(display (scheme-version)) (newline)\n' | scheme -q
            '';
          };
        }
      );

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
