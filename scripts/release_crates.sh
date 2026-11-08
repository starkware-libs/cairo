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

RETRY_COUNT=2
RATELIMIT_SLEEP=60

# Publish the crates.
for CRATE in "${CRATES[@]:$SKIP_FIRST}"; do
    for ((attempt = 0; attempt <= RETRY_COUNT; attempt++)); do
        output=$(cargo publish --package "$CRATE" 2>&1)
        if [ $? -eq 0 ]; then
            echo "Published $CRATE successfully."
            break
        fi
        if echo "$output" | grep -qi "already exists\|already uploaded"; then
            echo "Skipping $CRATE (already published)."
            break
        fi
        echo "$output" >&2
        if echo "$output" | grep -qi "Please try again after" && [ $attempt -lt $RETRY_COUNT ]; then
            echo "Rate limited; retrying $CRATE in ${RATELIMIT_SLEEP}s (attempt $attempt)..." >&2
            sleep $RATELIMIT_SLEEP
            continue
        fi
        exit 1
    done
done
