# An optional argument, '-s | --skip-first <#num_to_skip>', can be passed to skip the first #num_to_skip crates, otherwise SKIP_FIRST is set to 0.
if [ "$1" = "-s" ] || [ "$1" = "--skip-first" ]; then
    if [ -z "$2" ]; then
        echo "Error: --skip-first requires a numeric argument."
        exit 1
    fi
    SKIP_FIRST=$2
else
    SKIP_FIRST=0
fi

source "$(dirname "$0")/crates_list.sh"

# Publish the crates.
for CRATE in "${CRATES[@]:$SKIP_FIRST}"; do
    cargo publish --package "$CRATE" || exit 1
done
