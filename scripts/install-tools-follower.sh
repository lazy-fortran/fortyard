#!/bin/sh
# Run follow-tools.sh every five minutes on this host (systemd user timer on
# Linux, launchd agent on macOS), from its own fortyard clone, which it keeps
# at main; then once now.
set -eu
home=${FY_TOOLS_HOME:-$HOME/.cache/fy-tools}
src=$home/src/fortyard
mkdir -p "$home/src"
[ -d "$src/.git" ] || git clone -q https://github.com/lazy-fortran/fortyard.git "$src"
script=$src/scripts/follow-tools.sh
case "$(uname -s)" in
Darwin)
    plist=$HOME/Library/LaunchAgents/org.lazy-fortran.follow-tools.plist
    mkdir -p "$(dirname "$plist")"
    cat > "$plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>Label</key><string>org.lazy-fortran.follow-tools</string>
<key>ProgramArguments</key><array><string>/bin/sh</string><string>$script</string></array>
<key>EnvironmentVariables</key><dict><key>PATH</key><string>$PATH</string></dict>
<key>StartInterval</key><integer>300</integer>
<key>RunAtLoad</key><true/>
</dict></plist>
PLIST
    launchctl bootout "gui/$(id -u)" "$plist" 2>/dev/null || true
    launchctl bootstrap "gui/$(id -u)" "$plist"
    ;;
*)
    units=${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user
    mkdir -p "$units"
    cat > "$units/follow-tools.service" <<UNIT
[Unit]
Description=Keep fo and fy at lazy-fortran main

[Service]
Type=oneshot
Environment=PATH=$PATH
ExecStart=/bin/sh $script
UNIT
    cat > "$units/follow-tools.timer" <<UNIT
[Unit]
Description=Check lazy-fortran main for new fo and fy every five minutes

[Timer]
OnBootSec=1min
OnUnitActiveSec=5min

[Install]
WantedBy=timers.target
UNIT
    systemctl --user daemon-reload
    systemctl --user enable --now follow-tools.timer
    ;;
esac
/bin/sh "$script"
