
gfortran --version

# Detect if we need to change directory
# If we're already in the tests directory, stay here
# If we're in the repo root, go to tests
# If we're in a parent directory (like Docker setup), go to forutils/tests
if [ -f "run_tests.sh" ] && [ -f "Makefile" ]; then
    # We're already in the tests directory
    echo "Already in tests directory"
elif [ -d "tests" ] && [ -f "tests/run_tests.sh" ]; then
    # We're in the repo root, go to tests
    echo "Changing to tests directory"
    cd tests
elif [ -d "forutils/tests" ] && [ -f "forutils/tests/run_tests.sh" ]; then
    # We're in a parent directory (Docker setup), go to forutils/tests
    echo "Changing to forutils/tests directory"
    cd forutils/tests
else
    echo "Error: Cannot find tests directory"
    exit 1
fi

set -e
set -o pipefail

make

echo ''
echo 'Testing Debug build..'

./tester_Debug

echo ''
echo 'Testing Release build..'

./tester_Release

