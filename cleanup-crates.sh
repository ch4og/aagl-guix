#!/bin/sh
# SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
# SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
# SPDX-License-Identifier: GPL-3.0-or-later


FILE=aagl/packages/rust-crates.scm
PATTERN='^(define rust-'

grep "$PATTERN" $FILE | cut -d' ' -f2 | while IFS= read -r crate
do
    if [ "$(grep -wc "$crate" $FILE)" -eq 1 ]; then
        echo "\
(begin
  (use-modules (guix utils))
  (let ((source-properties
         (find-definition-location \"$FILE\" '$crate #:define-prefix 'define)))
    (and=> source-properties delete-expression)))" |
            guix repl -t machine
    fi
done

sed --in-place ':a;N;$!ba;s/\n\n\+/\n\n/g' $FILE
