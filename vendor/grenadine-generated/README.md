# vendor/grenadine-generated

The four Grenadine namespaces that upstream does **not** commit.

From v0.1.7 Grenadine generates `basis`, `coordinate`, `expander` and `gitlibs`
from pinned upstream sources (`clojure/tools.deps`, `clojure/tools.gitlibs`)
rather than keeping them in git — its own `.gitignore` lists all four. So the
`vendor/grenadine` submodule is an incomplete source tree on its own: even
Grenadine's committed `graph.cljc` requires `coordinate` and `expander`, and
jolt's `jolt.deps` requires `expander` directly.

Jolt puts `vendor/grenadine/src` on the source roots and loads Clojure from it.
The alternative to vendoring these four would be running Grenadine's
`util/source-patches` as part of every jolt build — it needs `yq`, and it
`git clone`s the pinned upstreams into a cache. That is a network dependency on
every build, every CI job and the release matrix, so it is not what we do.

## Where these came from

The `grenadine-<version>-src.tar.gz` asset of the matching Grenadine release,
which ships the generated sources alongside the committed ones. Verified
against the release's `grenadine-checksums.txt`:

    49c5f701e19d9a2a517581d0a4b222f2069209a9c2971d9f516b3dd15970c3e1  grenadine-0.1.13-src.tar.gz

The tarball's copies of the files Grenadine *does* commit are byte-identical to
the git tag, checked file by file — which is what makes taking half the tree
from the submodule and half from the tarball coherent rather than a guess.

## Refreshing on a bump

`VERSION` records the release these came from as `<tag> <sha>`, and
`make grenadinecheck` fails if the **sha** disagrees with the commit the
submodule is pinned to. Bumping the submodule without refreshing these files is
otherwise silent, and mixing two Grenadine versions is exactly the kind of
breakage that surfaces far from its cause.

The check compares the commit rather than the tag because a submodule checkout
does not necessarily carry tags — CI's does not, so `git describe` fails there.
A tag comparison passed on a developer machine and failed on CI, which is the
wrong way round for a gate.

    git -C vendor/grenadine checkout vX.Y.Z
    gh release download vX.Y.Z --repo clojurestar/grenadine \
      --pattern 'grenadine-*-src.tar.gz' --pattern 'grenadine-checksums.txt'
    shasum -a 256 -c grenadine-checksums.txt --ignore-missing
    tar xzf grenadine-X.Y.Z-src.tar.gz
    cp grenadine-X.Y.Z-src/src/grenadine/{basis,coordinate,expander,gitlibs}.cljc \
       vendor/grenadine-generated/grenadine/
    # REPLACE the version line, keeping the comment header — appending a
    # second one makes grenadinecheck read both shas as one string and fail.
    { grep '^#' vendor/grenadine-generated/VERSION
      printf 'vX.Y.Z %s\n' "$(git -C vendor/grenadine rev-parse HEAD)"
    } > /tmp/VERSION && mv /tmp/VERSION vendor/grenadine-generated/VERSION

If a future Grenadine generates a different set, `make grenadinecheck` will not
catch that on its own — the load will fail with an unresolved namespace, which
is loud enough, and this list gets updated in the same commit.
