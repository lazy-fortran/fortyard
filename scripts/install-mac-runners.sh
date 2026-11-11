#!/bin/sh
# Install the landing-check runners of a macOS (arm64) host as launchd
# daemons that start at boot and are restarted whenever they end:
#   install-mac-runners.sh SHARED TARGET COUNT [TOTAL_GIB [JOBS]]
# SHARED/runner-1 ... SHARED/runner-COUNT must each hold `token` (runner
# scope). The checks of all runners together stay within TOTAL_GIB of memory
# (default 16): 2 GiB are left to the runners and their builds of fy, the rest
# is split evenly between the checks. JOBS (default 8) is FO_JOBS of a check.
# Each check runs in the sandbox of sandbox-runtime (srt): it reads nothing
# of the user's home but the runner's directories and the toolchain, writes
# only its runner's work, home and the shared caches, and reaches only the
# hosts that fetch dependencies. Needs sudo for /Library/LaunchDaemons.
# Rerunning it updates the scripts and settings and restarts the runners.
set -eu
shared=$1 target=$2 count=$3 total=${4:-16} jobs=${5:-8}
here=$(cd "$(dirname "$0")/.." && pwd)
srt_version=0.0.82
user=$(id -un)
uhome=$HOME
memory=$(( (total - 2) / count ))
[ "$memory" -ge 1 ] || { echo "install-mac-runners: $total GiB is too little for $count runners" >&2; exit 1; }
export PATH="$uhome/.local/bin:/opt/homebrew/bin:$PATH"
command -v fo > /dev/null || { echo "install-mac-runners: fo is missing" >&2; exit 1; }
[ "$(srt --version 2> /dev/null || true)" = "$srt_version" ] ||
    npm install -g --silent "@anthropic-ai/sandbox-runtime@$srt_version"
mkdir -p "$shared/bin" "$shared/cache" "$shared/apple"
cp "$here/scripts/mac-runner.sh" "$shared/bin/mac-runner.sh"
chmod 755 "$shared/bin/mac-runner.sh"
# Local network privacy lets a launchd daemon reach hosts of its own subnet
# (the forge, for the Macs at TU Graz) only through Apple's binaries: other
# ones, like Homebrew's git, fail with EHOSTUNREACH. fy calls git and curl.
ln -sf /usr/bin/git /usr/bin/curl "$shared/apple/"
i=1
while [ "$i" -le "$count" ]; do
    home=$shared/runner-$i
    label=at.tugraz.fortyard.runner-$i
    [ -s "$home/token" ] || { echo "install-mac-runners: $home/token missing" >&2; exit 1; }
    chmod 600 "$home/token"
    mkdir -p "$home/tmp" "$home/home" "$home/data"
    cat > "$home/srt.json" <<JSON
{
  "network": {
    "allowedDomains": ["github.com", "*.github.com", "*.githubusercontent.com", "pypi.org",
      "files.pythonhosted.org", "code.tugraz.at", "gitlab.mpcdf.mpg.de"],
    "deniedDomains": []
  },
  "filesystem": {
    "denyRead": ["$uhome", "$home/token", "$home/git-credentials", "$home/data/credentials"],
    "allowRead": ["$home", "$shared/apple", "$shared/cache", "$shared/releases",
      "$uhome/.local/bin", "$uhome/.local/lib"],
    "allowWrite": ["$home/tmp", "$home/home", "$shared/cache"],
    "denyWrite": []
  }
}
JSON
    plist=/Library/LaunchDaemons/$label.plist
    cat > "$home/$label.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>$label</string>
  <key>UserName</key><string>$user</string>
  <key>ProgramArguments</key><array><string>$shared/bin/mac-runner.sh</string></array>
  <key>EnvironmentVariables</key>
  <dict>
    <key>HOME</key><string>$uhome</string>
    <key>PATH</key><string>$shared/apple:$uhome/.local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin</string>
    <key>LANG</key><string>en_US.UTF-8</string>
    <key>FY_TARGET</key><string>$target</string>
    <key>FY_RUNNER</key><string>$home</string>
    <key>FY_SHARED</key><string>$shared</string>
    <key>FY_CHECK_MEMORY</key><string>$memory</string>
    <key>FO_JOBS</key><string>$jobs</string>
  </dict>
  <key>RunAtLoad</key><true/>
  <key>KeepAlive</key><true/>
  <key>ThrottleInterval</key><integer>30</integer>
  <key>Nice</key><integer>10</integer>
  <key>StandardOutPath</key><string>$home/runner.log</string>
  <key>StandardErrorPath</key><string>$home/runner.log</string>
</dict>
</plist>
PLIST
    plutil -lint "$home/$label.plist" > /dev/null
    sudo launchctl bootout "system/$label" 2> /dev/null || true
    sudo install -m 644 -o root -g wheel "$home/$label.plist" "$plist"
    rm -f "$home/$label.plist"
    sudo launchctl bootstrap system "$plist"
    i=$((i + 1))
done
