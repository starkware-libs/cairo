pushd $(dirname "$0")/..

COLOR_OFF="\033[0m"
RED='\033[1;31m'
GREEN='\033[1;32m'
YELLOW='\033[1;33m'
PURPLE='\033[1;35m'


printf "${YELLOW}Check Line Length...\n"
scripts/line_length.py


printf "${YELLOW}Run black...\n"
black -l 100 --diff --check --color .
printf "${GREEN}Run black succeed\n"

printf "${YELLOW} pytest...\n"
pytest . -sv -n auto
PYTEST_EXIT_CODE=$?

if [ $PYTEST_EXIT_CODE -eq 0 ]; then
    printf "${GREEN}Pytest succeed\n"
else
    printf "${RED}Pytest failed with exit code $PYTEST_EXIT_CODE\n"
fi

# Reset
printf "${COLOR_OFF}"
popd

exit $PYTEST_EXIT_CODE
