#!/bin/sh
# Brings up the egress proxy (executor role) on a Docker network without a way
# out and checks its decisions: listed hosts and their subdomains pass; a TU
# Graz address, a private address, literal IPs, unlisted hosts, other ports
# and CONNECT to a port other than 443 are refused, also for an IP whose
# reverse name lies under a listed domain; the repository's extra
# hosts pass unless they point into TU Graz; a private repository loses the
# package hosts it does not name; learning mode lets unlisted public hosts
# pass and logs them; the log rotates. Needs docker; nothing leaves the test network.
#   test.sh [EGRESS_IMAGE]   (default: built from this directory)
set -eu
here=$(cd "$(dirname "$0")" && pwd)
web_image=${FY_EGRESS_TEST_WEB:-busybox:1.36}
curl_image=${FY_EGRESS_TEST_CURL:-curlimages/curl:8.10.1}
id=fy-egress-test-$$
image=${1:-}
work=$(mktemp -d "${TMPDIR:-/var/tmp}/fy-egress-test.XXXXXX")
cleanup() {
    docker rm -f "$id-web" "$id-client" "$id-strict" "$id-learn" >/dev/null 2>&1 || true
    docker network rm "$id" >/dev/null 2>&1 || true
    [ -z "${built:-}" ] || docker rmi "$image" >/dev/null 2>&1 || true
    rm -rf "$work"
}
trap cleanup EXIT INT TERM
if [ -z "$image" ]; then
    image=fortyard-egress:$id
    built=1
    docker build -q -t "$image" "$here" >/dev/null
fi

docker network create --internal "$id" >/dev/null
docker run -d --name "$id-web" --network "$id" --read-only --cap-drop ALL \
    --security-opt no-new-privileges:true --tmpfs /tmp --user 65534:65534 \
    --sysctl net.ipv4.ip_unprivileged_port_start=0 --entrypoint sh "$web_image" -c \
    'mkdir /tmp/www && echo reached > /tmp/www/index.html &&
        httpd -p 443 -h /tmp/www && exec httpd -f -p 80 -h /tmp/www' >/dev/null
web=$(docker inspect -f "{{(index .NetworkSettings.Networks \"$id\").IPAddress}}" "$id-web")
docker run -d --name "$id-client" --network "$id" --cap-drop ALL --entrypoint sleep \
    "$curl_image" infinity >/dev/null

# Names resolve through the proxy's /etc/hosts: listed and unlisted names
# point to the test server, the others to addresses of each kind.
mkdir -p "$work/lists" "$work/extra" "$work/log-strict" "$work/log-learn"
printf '# test\nAllowed.Example\nlisted-tu.example\n' > "$work/lists/executor.txt"
printf 'pkg.example\n' > "$work/lists/packages.txt"
printf 'extra.example\nextra-tu.example\n' > "$work/extra/executor.txt"
chmod -R a+rX "$work"
chmod a+rwx "$work/log-strict" "$work/log-learn"
hosts="--add-host allowed.example:$web --add-host sub.allowed.example:$web
    --add-host unlisted.example:$web --add-host tu.example:129.27.1.1
    --add-host listed-tu.example:129.27.1.2 --add-host extra-tu.example:129.27.1.3
    --add-host priv.example:10.1.2.3 --add-host public.example:192.0.2.1
    --add-host extra.example:192.0.2.2 --add-host pkg.example:192.0.2.3
    --add-host evil.allowed.example:192.0.2.77 --add-host evil.pkg.example:192.0.2.78"
proxy() {
    # shellcheck disable=SC2086
    docker run -d --name "$id-$1" --network "$id" --read-only --cap-drop ALL \
        --security-opt no-new-privileges:true --tmpfs /tmp --user "$(id -u):$(id -g)" \
        $hosts -e FY_EGRESS_ROLE=executor -e FY_EGRESS_LEARN="$2" \
        -e FY_EGRESS_LISTS=/lists -e FY_EGRESS_EXTRA=/extra/executor.txt -e FY_EGRESS_LOG=/log \
        -v "$work/lists:/lists:ro" -v "$work/extra:/extra:ro" -v "$work/log-$1:/log" \
        "$image" >/dev/null
}
proxy strict 0
proxy learn 1

fails=0
expect() { # proxy what expected-code curl-arguments...
    p=$1 what=$2 want=$3
    shift 3
    got=$(docker exec "$id-client" curl -s -o /dev/null -m 20 -x "http://$id-$p:3128" \
        -w '%{http_connect} %{http_code}' "$@" || true)
    case " $got" in
        *" $want"*) echo "ok   $p: $what ($got)" ;;
        *) echo "FAIL $p: $what: wanted $want, got $got"; fails=$((fails + 1)) ;;
    esac
}
for p in strict learn; do
    i=0
    until docker exec "$id-client" curl -s -o /dev/null -m 5 -x "http://$id-$p:3128" \
        -w '%{http_code}' http://192.0.2.9/ 2>/dev/null | grep -q 403; do
        i=$((i + 1))
        if [ $i -ge 120 ] || [ -z "$(docker ps -q -f name="^$id-$p\$")" ]; then
            docker logs "$id-$p" 2>&1 | tail -20
            echo "FAIL proxy $p did not start"
            exit 1
        fi
        docker exec "$id-client" sleep 1
    done
done

# 403 is squid's refusal; 503 (connection failed: the test network has no
# route out) means the proxy let the request through.
expect strict 'a listed host is reached' '000 200' http://allowed.example/
expect strict 'a listed host is reached over CONNECT to 443' '200 200' \
    -p http://allowed.example:443/
expect strict 'a subdomain of a listed host passes' '000 200' http://sub.allowed.example/
expect strict 'a listed name in TU Graz passes (lists come first)' '503' http://listed-tu.example/
expect strict 'a TU Graz address is refused' '403' http://tu.example/
expect strict 'a private address is refused' '403' http://priv.example/
expect strict 'an unlisted host on a private address is refused' '403' http://unlisted.example/
expect strict 'an unlisted public host is refused' '403' http://public.example/
expect strict 'a literal IPv4 address is refused' '403' http://192.0.2.1/
expect strict 'a literal IPv6 address is refused' '403' 'http://[2001:db8::1]/'
expect strict 'a literal TU Graz address is refused' '403' http://129.27.2.3/
# squid finds a reverse name in /etc/hosts as it would in DNS, which the
# owner of an address can set to a name under a listed domain.
expect strict 'a literal IP with a listed reverse name is refused' '403' http://192.0.2.77/
expect strict 'CONNECT to an IP with a listed reverse name is refused' '403' \
    -p http://192.0.2.77:443/
expect strict 'a literal IP with a package host as reverse name is refused' '403' \
    http://192.0.2.78/
expect strict 'a port other than 80 and 443 is refused' '403' http://allowed.example:8080/
expect strict 'CONNECT to port 80 is refused' '403' -p http://allowed.example:80/
expect strict 'a package host passes' '503' http://pkg.example/
expect strict 'an extra host of the repository passes' '503' http://extra.example/
expect strict 'an extra host in TU Graz is refused' '403' http://extra-tu.example/
printf 'private\nextra.example\n' > "$work/extra/executor.txt"
expect strict 'a private repository has no package host it does not name' '403' http://pkg.example/
expect strict 'a private repository keeps its extra hosts' '503' http://extra.example/
printf 'private\npkg.example\n' > "$work/extra/executor.txt"
expect strict 'a private repository has the package hosts it names' '503' http://pkg.example/
rm "$work/extra/executor.txt"
expect strict 'without a check, extra hosts are refused' '403' http://extra.example/
expect learn 'learning lets an unlisted public host pass' '503' http://public.example/
expect learn 'learning still refuses a TU Graz address' '403' http://tu.example/
expect learn 'learning still refuses a private address' '403' http://priv.example/
expect learn 'learning still refuses a literal IP' '403' http://192.0.2.1/
expect learn 'learning refuses a literal IP with a listed reverse name' '403' http://192.0.2.77/
expect learn 'learning still refuses other ports' '403' http://public.example:8080/

check_log() { # proxy what pattern present(1/0)
    if grep -q "$3" "$work/log-$1/unlisted.log" 2>/dev/null; then has=1; else has=0; fi
    if [ "$has" = "$4" ]; then echo "ok   $1: $2"; else
        echo "FAIL $1: $2"; cat "$work/log-$1/unlisted.log" 2>&1 | tail -5; fails=$((fails + 1))
    fi
}
check_log strict 'a refused unlisted host is logged' 'TCP_DENIED/403 GET http://public.example/' 1
check_log strict 'a listed host is not logged as unlisted' 'allowed.example' 0
check_log learn 'a host passed by learning is logged' '/503 GET http://public.example/' 1

# The log of unlisted hosts is rotated (daily by the container) and kept.
docker exec "$id-strict" squid -k rotate -f /tmp/squid.conf
i=0
until [ -s "$work/log-strict/unlisted.log.0" ]; do
    i=$((i + 1))
    [ $i -lt 120 ] || break
    docker exec "$id-client" sleep 1
done
if [ -s "$work/log-strict/unlisted.log.0" ]; then echo "ok   strict: the log of unlisted hosts rotates"
else echo "FAIL strict: the log of unlisted hosts rotates"; fails=$((fails + 1)); fi

[ "$fails" = 0 ] || { echo "$fails failed"; exit 1; }
echo "egress proxy: all checks passed"
