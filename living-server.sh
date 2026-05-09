#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Check for ANTHROPIC_API_KEY
if [ -z "$ANTHROPIC_API_KEY" ]; then
  echo "Warning: ANTHROPIC_API_KEY is not set. Claude integration will not work."
  echo "Set it with: export ANTHROPIC_API_KEY=your-key-here"
  echo ""
fi

# Check dependencies
if ! command -v sbcl &> /dev/null; then
  echo "Error: sbcl not found. Install SBCL first."
  exit 1
fi

if ! command -v cabal &> /dev/null; then
  echo "Error: cabal not found. Install GHC + cabal first."
  exit 1
fi

echo "=== Living Server ==="
echo ""

# Build the Haskell dashboard if needed
echo "Building dashboard..."
cd "$SCRIPT_DIR/dashboard"
cabal build 2>&1 | tail -5
DASHBOARD_BIN=$(cabal list-bin dashboard 2>/dev/null)

if [ -z "$DASHBOARD_BIN" ] || [ ! -f "$DASHBOARD_BIN" ]; then
  echo "Error: Dashboard build failed."
  exit 1
fi

echo "Dashboard built successfully."
echo ""

# Run the dashboard (which spawns SBCL internally)
cd "$SCRIPT_DIR"
echo "Starting Living Server..."
echo "Dashboard: http://localhost:8080"
echo "User Server: http://localhost:3001"
echo ""

# Clean up all child processes on exit (Ctrl-C, terminal close, etc.)
cleanup() {
  echo ""
  echo "Shutting down Living Server..."
  # Kill all processes in our process group
  kill -- -$$ 2>/dev/null
  # Also kill any sbcl children directly in case they escaped the group
  pkill -P $$ sbcl 2>/dev/null
  wait 2>/dev/null
  echo "Done."
}
trap cleanup EXIT INT TERM HUP

# Open browser after a short delay
(sleep 3 && open "http://localhost:8080" 2>/dev/null || xdg-open "http://localhost:8080" 2>/dev/null) &

"$DASHBOARD_BIN" &
DASHBOARD_PID=$!
wait $DASHBOARD_PID
