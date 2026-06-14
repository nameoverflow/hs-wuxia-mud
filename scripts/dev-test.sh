#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

SERVER_HOST="127.0.0.1"
SERVER_PORT="9160"
CLIENT_HOST="${CLIENT_HOST:-127.0.0.1}"
CLIENT_PORT="${CLIENT_PORT:-8080}"
TEST_USER="${TEST_USER:-tester}"
TEST_RESET="${TEST_RESET:-1}"
OPEN_BROWSER="${OPEN_BROWSER:-1}"
SKIP_BUILD="${SKIP_BUILD:-0}"
SKIP_CLIENT_INSTALL="${SKIP_CLIENT_INSTALL:-0}"

SERVER_PID=""
CLIENT_PID=""

usage() {
  cat <<EOF
Usage: scripts/dev-test.sh [options]

Starts the Haskell dev server with MUD_DEV_MODE=1 and the Svelte client dev
server, then opens the auto-login test URL.

Options:
  --no-open       Print the test URL without opening a browser.
  --no-reset      Auto-login without resetting the test save.
  --skip-build    Skip stack build before starting the server.
  --user NAME     Use a test username other than "tester".
  -h, --help      Show this help.

Environment:
  CLIENT_HOST=127.0.0.1
  CLIENT_PORT=8080
  TEST_USER=tester
  TEST_RESET=1
  OPEN_BROWSER=1
  SKIP_BUILD=0
  SKIP_CLIENT_INSTALL=0
EOF
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --no-open)
      OPEN_BROWSER=0
      ;;
    --no-reset)
      TEST_RESET=0
      ;;
    --skip-build)
      SKIP_BUILD=1
      ;;
    --user)
      if [ "$#" -lt 2 ]; then
        echo "Missing value for --user" >&2
        exit 2
      fi
      TEST_USER="$2"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    echo "Required command not found: $1" >&2
    exit 1
  fi
}

cleanup() {
  trap - EXIT INT TERM
  if [ -n "$CLIENT_PID" ] && kill -0 "$CLIENT_PID" >/dev/null 2>&1; then
    kill "$CLIENT_PID" >/dev/null 2>&1 || true
  fi
  if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" >/dev/null 2>&1; then
    kill "$SERVER_PID" >/dev/null 2>&1 || true
  fi
  [ -n "$CLIENT_PID" ] && wait "$CLIENT_PID" >/dev/null 2>&1 || true
  [ -n "$SERVER_PID" ] && wait "$SERVER_PID" >/dev/null 2>&1 || true
}

check_process_alive() {
  local pid="$1"
  local name="$2"
  if ! kill -0 "$pid" >/dev/null 2>&1; then
    wait "$pid" || true
    echo "$name exited before becoming ready." >&2
    exit 1
  fi
}

wait_for_tcp() {
  local name="$1"
  local host="$2"
  local port="$3"
  local pid="$4"
  local attempts=0

  if ! command -v nc >/dev/null 2>&1; then
    sleep 1
    check_process_alive "$pid" "$name"
    return
  fi

  while [ "$attempts" -lt 80 ]; do
    if nc -z "$host" "$port" >/dev/null 2>&1; then
      return
    fi
    check_process_alive "$pid" "$name"
    attempts=$((attempts + 1))
    sleep 0.25
  done

  echo "$name did not listen on $host:$port in time." >&2
  exit 1
}

wait_for_http() {
  local name="$1"
  local url="$2"
  local pid="$3"
  local attempts=0

  if ! command -v curl >/dev/null 2>&1; then
    sleep 1
    check_process_alive "$pid" "$name"
    return
  fi

  while [ "$attempts" -lt 80 ]; do
    if curl -fsS "$url" >/dev/null 2>&1; then
      return
    fi
    check_process_alive "$pid" "$name"
    attempts=$((attempts + 1))
    sleep 0.25
  done

  echo "$name did not serve $url in time." >&2
  exit 1
}

open_test_url() {
  local url="$1"
  if [ "$OPEN_BROWSER" != "1" ]; then
    return
  fi

  if command -v open >/dev/null 2>&1; then
    open "$url" >/dev/null 2>&1 || true
  elif command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$url" >/dev/null 2>&1 || true
  elif [ -n "${BROWSER:-}" ]; then
    "$BROWSER" "$url" >/dev/null 2>&1 || true
  fi
}

require_cmd stack
require_cmd npm

cd "$ROOT_DIR"

if [ "$SKIP_BUILD" != "1" ]; then
  echo "[dev-test] Building server executable..."
  stack build mud-hs:exe:mud-hs-exe
fi

if [ "$SKIP_CLIENT_INSTALL" != "1" ] && [ ! -d "$ROOT_DIR/client/node_modules" ]; then
  echo "[dev-test] Installing client dependencies..."
  npm --prefix "$ROOT_DIR/client" ci
fi

TEST_URL="http://${CLIENT_HOST}:${CLIENT_PORT}/?test=1&user=${TEST_USER}"
if [ "$TEST_RESET" = "0" ]; then
  TEST_URL="${TEST_URL}&reset=0"
fi

trap cleanup EXIT INT TERM

echo "[dev-test] Starting server on ${SERVER_HOST}:${SERVER_PORT} with MUD_DEV_MODE=1..."
MUD_DEV_MODE=1 stack exec mud-hs-exe &
SERVER_PID=$!
wait_for_tcp "Server" "$SERVER_HOST" "$SERVER_PORT" "$SERVER_PID"

echo "[dev-test] Starting client on http://${CLIENT_HOST}:${CLIENT_PORT}..."
npm --prefix "$ROOT_DIR/client" run dev -- --host "$CLIENT_HOST" --port "$CLIENT_PORT" --strictPort &
CLIENT_PID=$!
wait_for_http "Client" "http://${CLIENT_HOST}:${CLIENT_PORT}/" "$CLIENT_PID"

echo "[dev-test] Ready: ${TEST_URL}"
open_test_url "$TEST_URL"
echo "[dev-test] Press Ctrl-C to stop server and client."

while true; do
  check_process_alive "$SERVER_PID" "Server"
  check_process_alive "$CLIENT_PID" "Client"
  sleep 1
done
