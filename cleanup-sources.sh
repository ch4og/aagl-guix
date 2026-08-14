#!/bin/sh
# SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
# SPDX-License-Identifier: GPL-3.0-or-later

set -eu

CRATES=${CRATES:-aagl/packages/rust-crates.scm}
SOURCES=${SOURCES:-aagl/packages/rust-sources.scm}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

ROOTS=$({
    sed -n 's/.*make-aagl[[:space:]]*#:name[[:space:]]*"\([^"]*\)".*/\1/p' \
        aagl/packages/*.scm
    sed -n "s/.*aagl-cargo-inputs[[:space:]]*'\([[:alnum:]-]*\).*/\1/p" \
        aagl/packages/*.scm
} | sort -u | tr '\n' ' ')

: > "$TMP/dead"
awk -v roots="$ROOTS" -v dead="$TMP/dead" '
NR == FNR {
    if ($0 ~ /^\(define-public rust-/) {
        source = $2
        key = source
        sub(/^rust-/, "", key)
        sub(/\.[[:xdigit:]]{7}$/, "", key)
        source_key[source] = key
    }
    next
}
/^[[:space:]]*\(define-cargo-inputs[[:space:]]+lookup-cargo-inputs$/ {
    table = 1
    next
}
table && /^[[:space:]]+\([^[:space:]]+[[:space:]]+=>$/ {
    key = $1
    sub(/^\(/, "", key)
    next
}
table {
    rest = $0
    while (match(rest, /rust-[[:alnum:]_.+:-]+/)) {
        edge[key, ++edges[key]] = substr(rest, RSTART, RLENGTH)
        rest = substr(rest, RSTART + RLENGTH)
    }
}
END {
    count = split(roots, root, " ")
    for (i = 1; i <= count; i++)
        if (root[i] != "")
            live[root[i]] = 1

    for (head = 1; head <= count; head++) {
        key = root[head]
        for (i = 1; i <= edges[key]; i++) {
            dependency = edge[key, i]
            if ((dependency in source_key) &&
                !live[source_key[dependency]]) {
                live[source_key[dependency]] = 1
                root[++count] = source_key[dependency]
            }
        }
    }

    for (source in source_key)
        if (!live[source_key[source]])
            print source > dead
}' "$SOURCES" "$CRATES"

awk -v dead="$TMP/dead" '
BEGIN {
    while ((getline source < dead) > 0) {
        sub(/^rust-/, "", source)
        sub(/\.[[:xdigit:]]{7}$/, "", source)
        remove[source] = 1
    }
}
/^[[:space:]]*\(define-cargo-inputs[[:space:]]+lookup-cargo-inputs$/ {
    table = 1
}
table && /^[[:space:]]+\([^[:space:]]+[[:space:]]+=>$/ {
    key = $1
    sub(/^\(/, "", key)
    remove_entry = (key in remove)
}
!remove_entry { print }
' "$CRATES" > "$TMP/crates"
mv "$TMP/crates" "$CRATES"

delete_definition() {
    guix repl -t machine >/dev/null <<EOF
(use-modules (guix utils))
(and=> (find-definition-location "$1" '$2 #:define-prefix '$3)
       delete-expression)
EOF
}

while IFS= read -r source
 do
    delete_definition "$SOURCES" "$source" define-public
    delete_definition "$CRATES" "$source" define
    grep -F -v "$source" "$CRATES" > "$TMP/crates" || :
    mv "$TMP/crates" "$CRATES"
done < "$TMP/dead"

sed --in-place ':a;N;$!ba;s/\n\n\+/\n\n/g' "$CRATES" "$SOURCES"
