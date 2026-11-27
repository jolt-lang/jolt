#!/bin/sh
# apt-get update for CI runners. The runner image ships third-party apt sources
# (packages.microsoft.com) that jolt installs nothing from, and one answering
# 403 failed `apt-get update`, and the whole job with it, before any gate ran.
# Drop those sources, then retry the update a few times for a mirror blip.
set -e
sudo rm -f /etc/apt/sources.list.d/microsoft-prod.list \
           /etc/apt/sources.list.d/microsoft-prod.sources \
           /etc/apt/sources.list.d/azure-cli.list \
           /etc/apt/sources.list.d/azure-cli.sources
i=1
while :; do
  if sudo apt-get update; then exit 0; fi
  [ "$i" -ge 3 ] && exit 1
  echo "apt-get update failed (attempt $i of 3), retrying"
  i=$((i + 1))
  sleep 10
done
