#!/usr/bin/env bash

# Read the Gopher selector (the line after client connects)
read selector

############################################
# process_response: Forwards the given
# selector to a container, rewrites links
# so that /... becomes /~/username/... etc.
############################################
process_response() {
    local container_host="$1"
    local prefix="$2"
    local stripped_selector="$3"

    # The container's internal port for bore
    local port="7071"

    # Example placeholders for rewriting
    local target_host="gopher.someodd.zip"
    local target_port="70"

    # Forward the stripped_selector to the container on port 7071
    # Then rewrite any links (sed) that reference target_host:7071 to
    # reflect our gopher "prefix" or change the port if needed.
    echo "$stripped_selector" | nc "$container_host" "$port" | \
    sed -e "/\t${target_host}\t${port}/s|\t/|\t${prefix}/|g" \
        -e "s|\(${target_host}\)\t${port}|\1\t${target_port}|g"
}

############################################
# process_boreguest: Extract username from
# /~/username, then use process_response()
# to forward requests with path rewriting.
############################################
process_boreguest() {
    # Extract 'username' from /~/username
    local username
    username=$(echo "$selector" | awk -F'/' '{print $3}' | sed 's/^~//')

    # Strip "/~/username" from the selector, leaving remainder
    local stripped_selector
    stripped_selector=$(echo "$selector" | sed "s|^/~/[^/]*||")

    # Default to "/" if empty
    if [[ -z "$stripped_selector" ]]; then
        stripped_selector="/"
    fi

    # Now call process_response so we can rewrite any embedded links
    # Container = "boreguest_<username>"
    # Prefix    = "/~/<username>"  (so embedded links become /~/bob/stuff, etc.)
    # Selector  = the stripped selector we want to send
    process_response "boreguest_${username}" "/~/$username" "$stripped_selector"
}


############################################
# Main logic
############################################
case "$selector" in
    "/~/"*)
        process_boreguest
        ;;
    *)
        # Fallback or default “page”
        echo "iWelcome to the default gopherrouter!\tFakeHost\t1"
        echo "iTry /~/someuser to reach that user’s container.\tFakeHost\t1"
        echo "."
        ;;
esac
