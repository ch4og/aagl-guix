;;; SPDX-FileCopyrightText: 2025 Hilton Chain <hako@ultrarare.space>
;;; SPDX-FileCopyrightText: 2026 Nikita Mitasov <me@ch4og.com>
;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (aagl packages rust-crates)
  #:use-module (gnu packages commencement)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages rust-sources)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (aagl utils cargo)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

(define* (make-anime-game-core #:key version commit sha)
  (let ((name "rust-anime-game-core")
        (crate-symbol 'anime-game-core)
        (github-url "https://github.com/an-anime-team/anime-game-core"))
    (package
      (name name)
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url github-url) (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 sha))))
      (build-system cargo-build-system)
      (arguments
       (list #:install-source? #t
             #:skip-build? #t
             #:cargo-package-crates
             ''("anime-game-core")))
      (inputs (aagl-cargo-inputs crate-symbol))
      (home-page github-url)
      (synopsis "Unified library to control different games installations.")
      (description "Unified library to controll different games installations.
Provides basic instruments for adding support for mechanics like game updating.")
      (license license:gpl3))))

(define* (make-anime-launcher-sdk #:key version commit sha)
  (let ((name "rust-anime-launcher-sdk")
        (crate-symbol 'anime-launcher-sdk)
        (github-url "https://github.com/an-anime-team/anime-launcher-sdk"))
    (package
      (name name)
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url github-url) (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 sha))))
      (build-system cargo-build-system)
      (arguments
       (list #:install-source? #t
             #:skip-build? #t
             #:cargo-package-crates
             ''("anime-launcher-sdk")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'use-guix-vendored-dependencies
                   (lambda _
                     (substitute* "Cargo.toml"
                       (("tag =.*") "version = \"*\"\n")
                       (("^git = .*") "")))))))
      (inputs (aagl-cargo-inputs crate-symbol))
      (home-page github-url)
      (synopsis "Anime Game Launcher development SDK")
      (description "SDK based on anime-game-core with basic instruments like launcher
state system and configuration file manager, written in Rust")
      (license license:gpl3))))

(define rust-anime-game-core-1.36.3.044a1e8
  (make-anime-game-core
   #:version "1.36.3"
   #:commit "044a1e83782fb853a9cfa8bcb807689a5c9d73ae"
   #:sha "156s4daz89048r7bhk8ibr5rb07maya0fypvigkyn2dxldjjj46j"))

(define rust-anime-game-core-1.37.2.43f2d19
  (make-anime-game-core
   #:version "1.37.2"
   #:commit "43f2d196ec7eb5c60ec61d5ec94f22fea9bda1f3"
   #:sha "0z6k9p42fx55pvrhxk9y4v5giqsd34qvyrc6pqs41vl2wy5q21yx"))

(define rust-anime-launcher-sdk-1.32.0.87c4206
  (make-anime-launcher-sdk
   #:version "1.32.0"
   #:commit "87c42064d8422a39b92efbd9035cbd38fffe8f91"
   #:sha "189nsrm41ihhbg85qxvr650nhi7s4c2cj8m5sgmjbw0k7jfrq75j"))

(define rust-anime-launcher-sdk-1.33.0.f75593b
  (make-anime-launcher-sdk
   #:version "1.33.0"
   #:commit "f75593be9df416dd76ac6c5ce28140ed0feaef4d"
   #:sha "0jrrgpsc4syp3r7v0afnaigl2g4sj5y06fjy2anykllavsngmj55"))

(define rust-anime-launcher-sdk-1.34.3.04a6649
  (make-anime-launcher-sdk
   #:version "1.34.3"
   #:commit "04a66499b539d810a1f31f3cb2b8fbf62c9b7e73"
   #:sha "1pqr7zi1n0xdmhwp3gfzpkr8nppzzw6l61ms2d0fy2bcl1dhsfhb"))

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-addr2line-0.24.2
  (crate-source "addr2line" "0.24.2"
                "1hd1i57zxgz08j6h5qrhsnm2fi0bcqvsh389fw400xm3arz2ggnz"))

(define rust-addr2line-0.25.1
  (crate-source "addr2line" "0.25.1"
                "0jwb96gv17vdr29hbzi0ha5q6jkpgjyn7rjlg5nis65k41rk0p8v"))

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aes-0.8.4
  (crate-source "aes" "0.8.4"
                "1853796anlwp4kqim0s6wm1srl4ib621nm0cl2h3c8klsjkgfsdi"))

(define rust-ahash-0.8.12
  (crate-source "ahash" "0.8.12"
                "0xbsp9rlm5ki017c0w6ay8kjwinwm8knjncci95mii30rmwz25as"))

(define rust-aho-corasick-1.1.3
  (crate-source "aho-corasick" "1.1.3"
                "05mrpkvdgp5d20y2p989f187ry9diliijgwrs254fs9s1m1x6q4f"))

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-anstream-0.6.19
  (crate-source "anstream" "0.6.19"
                "0crr9a207dyn8k66xgvhvmlxm9raiwpss3syfa35c6265s9z26ih"))

(define rust-anstream-0.6.20
  (crate-source "anstream" "0.6.20"
                "14k1iqdf3dx7hdjllmql0j9sjxkwr1lfdddi3adzff0r7mjn7r9s"))

(define rust-anstyle-1.0.11
  (crate-source "anstyle" "1.0.11"
                "1gbbzi0zbgff405q14v8hhpi1kz2drzl9a75r3qhks47lindjbl6"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-query-1.1.3
  (crate-source "anstyle-query" "1.1.3"
                "1sgs2hq54wayrmpvy784ww2ccv9f8yhhpasv12z872bx0jvdx2vc"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anstyle-wincon-3.0.9
  (crate-source "anstyle-wincon" "3.0.9"
                "10n8mcgr89risdf35i73zc67aaa392bhggwzqlri1fv79297ags0"))

(define rust-anyhow-1.0.100
  (crate-source "anyhow" "1.0.100"
                "0qbfmw4hhv2ampza1csyvf1jqjs2dgrj29cv3h3sh623c6qvcgm2"))

(define rust-anyhow-1.0.101
  (crate-source "anyhow" "1.0.101"
                "1skmg90fnjnlgs3vl7bksw7036d3rqwqj20n2fxd2ppg67p0y3jz"))

(define rust-anyhow-1.0.98
  (crate-source "anyhow" "1.0.98"
                "11ylvjdrcjs0q9jgk1af4r5cx1qppj63plxqkq595vmc24rjsvg1"))

(define rust-arbitrary-1.4.1
  (crate-source "arbitrary" "1.4.1"
                "08zj2yanll5s5gsbmvgwvbq39iqzy3nia3yx3db3zwba08yhpqnx"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-ashpd-0.11.0
  (crate-source "ashpd" "0.11.0"
                "1pwrxm1dky7i0xb1wj5k5rfx7hhx06vks2d2dvpamlvzsw8g7gbc"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-async-trait-0.1.88
  (crate-source "async-trait" "0.1.88"
                "1dgxvz7g75cmz6vqqz0mri4xazc6a8xfj1db6r9fxz29lzyd6fg5"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-backtrace-0.3.75
  (crate-source "backtrace" "0.3.75"
                "00hhizz29mvd7cdqyz5wrj98vqkihgcxmv2vl7z0d0f53qrac1k8"))

(define rust-backtrace-0.3.76
  (crate-source "backtrace" "0.3.76"
                "1mibx75x4jf6wz7qjifynld3hpw3vq6sy3d3c9y5s88sg59ihlxv"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bitflags-2.10.0
  (crate-source "bitflags" "2.10.0"
                "1lqxwc3625lcjrjm5vygban9v8a6dlxisp1aqylibiaw52si4bl1"))

(define rust-bitflags-2.9.1
  (crate-source "bitflags" "2.9.1"
                "0rz9rpp5wywwqb3mxfkywh4drmzci2fch780q7lifbf6bsc5d3hv"))

(define rust-bitflags-2.9.4
  (crate-source "bitflags" "2.9.4"
                "157kkcv8s7vk6d17dar1pa5cqcz4c8pdrn16wm1ld7jnr86d2q92"))

(define rust-blake3-1.8.2
  (crate-source "blake3" "1.8.2"
                "1854x65zmjh9w9cfhyyyg0wmm2k5d87l13l4m7y40ajbkslam21q"))

(define rust-blake3-1.8.3
  (crate-source "blake3" "1.8.3"
                "0b9ay320z90xs5hyk48l1v3208yyvdy3gs3nnlb7xyxkaxyyys14"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.6.1
  (crate-source "block2" "0.6.1"
                "1wnwha7wjjqiamj9abq5l45fyzdxna2k2la0rp9w2hravc5jy39l"))

(define rust-bstr-1.12.0
  (crate-source "bstr" "1.12.0"
                "195i0gd7r7jg7a8spkmw08492n7rmiabcvz880xn2z8dkp8i6h93"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bumpalo-3.19.1
  (crate-source "bumpalo" "3.19.1"
                "044555i277xcinmqs7nnv8n5y4fqfi4l4lp1mp3i30vsidrxrnax"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

(define rust-bytes-1.10.1
  (crate-source "bytes" "1.10.1"
                "0smd4wi2yrhp5pmq571yiaqx84bjqlm1ixqhnvfwzzc6pqkn26yp"))

(define rust-bytes-1.11.1
  (crate-source "bytes" "1.11.1"
                "0czwlhbq8z29wq0ia87yass2mzy1y0jcasjb8ghriiybnwrqfx0y"))

(define rust-bzip2-0.4.4
  (crate-source "bzip2" "0.4.4"
                "1y27wgqkx3k2jmh4k26vra2kqjq1qc1asww8hac3cv1zxyk1dcdx"))

(define rust-bzip2-0.5.2
  (crate-source "bzip2" "0.5.2"
                "0iya6nbj0p2y8jss0z05yncc5hadry164fw3zva01y06v4igpv29"))

(define rust-bzip2-sys-0.1.13+1.0.8
  ;; TODO: Check bundled sources.
  (crate-source "bzip2-sys" "0.1.13+1.0.8"
                "056c39pgjh4272bdslv445f5ry64xvb0f7nph3z7860ln8rzynr2"))

(define rust-cached-0.53.1
  (crate-source "cached" "0.53.1"
                "0ik8fr14wlfd2mfh3jw3p3dcd8dq0bmrmhngwifwya3bmrak3mxl"))

(define rust-cached-0.55.1
  (crate-source "cached" "0.55.1"
                "055widjccy8z92jn41iz86aq75jf89207nd9ripk30w7gwlrr0xh"))

(define rust-cached-proc-macro-0.23.0
  (crate-source "cached_proc_macro" "0.23.0"
                "1ajxgl0w9vm55dk47qb0cq1akzncrwqcy78y37idq41dxm2s2hig"))

(define rust-cached-proc-macro-0.24.0
  (crate-source "cached_proc_macro" "0.24.0"
                "00y6ln647l6fcxlg563wncs1dqsbvjfbgqdkxdl1nwgh6kcr4fb7"))

(define rust-cached-proc-macro-types-0.1.1
  (crate-source "cached_proc_macro_types" "0.1.1"
                "1h3gw61v1inay4g3b8pirxlz18m81k63dw2q18zj9fnmidmkds5d"))

(define rust-cairo-rs-0.20.12
  (crate-source "cairo-rs" "0.20.12"
                "1l5d1rgvagvvs4a99i28ciyhdygf9v8hhy8mpk5akbr59q7vvqwi"))

(define rust-cairo-sys-rs-0.20.10
  (crate-source "cairo-sys-rs" "0.20.10"
                "12sgv9mimxy5nsxm4ipga1k7an59wn444xa7kbywp64qai3cg705"))

(define rust-cc-1.2.29
  (crate-source "cc" "1.2.29"
                "0qlkaspjmywvjyfqhpv2x4kwrqs6b69zg33wfi2l8fg2im9rj5aw"))

(define rust-cc-1.2.38
  (crate-source "cc" "1.2.38"
                "1sg7gd94611qhryvb0iip0zibjnhf1yha2wnp0pw2mgrd3himx40"))

(define rust-cc-1.2.56
  (crate-source "cc" "1.2.56"
                "1chvh9g2izhqad7vzy4cc7xpdljdvqpsr6x6hv1hmyqv3mlkbgxf"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-expr-0.20.1
  (crate-source "cfg-expr" "0.20.1"
                "08mzrbk0g69lya7wfa63rxds2bacbckkd5yl3kq39yaqkn4900qd"))

(define rust-cfg-expr-0.20.3
  (crate-source "cfg-expr" "0.20.3"
                "0p0y56pd9yns6wsi7slxv0xhv22ds0qf5j5138sjbhjyy8xmyb0s"))

(define rust-cfg-if-1.0.1
  (crate-source "cfg-if" "1.0.1"
                "0s0jr5j797q1vqjcd41l0v5izlmlqm7lxy512b418xz5r65mfmcm"))

(define rust-cfg-if-1.0.3
  (crate-source "cfg-if" "1.0.3"
                "1afg7146gbxjvkbjx7i5sdrpqp9q5akmk9004fr8rsm90jf2il9g"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-concurrent-queue-2.5.0
  (crate-source "concurrent-queue" "2.5.0"
                "0wrr3mzq2ijdkxwndhf79k952cp4zkz35ray8hvsxl96xrx1k82c"))

(define rust-constant-time-eq-0.3.1
  (crate-source "constant_time_eq" "0.3.1"
                "19nwwczii762pwlsm7bpizgjg8hkg1kqi32b2g4rglijklsbhx3w"))

(define rust-constant-time-eq-0.4.2
  (crate-source "constant_time_eq" "0.4.2"
                "16zamq60dq80k3rqlzh9j9cpjhishmh924lnwbplgrnmkkvfylix"))

(define rust-core-foundation-0.9.4
  (crate-source "core-foundation" "0.9.4"
                "13zvbbj07yk3b61b8fhwfzhy35535a583irf23vlcg59j7h9bqci"))

(define rust-core-foundation-sys-0.8.7
  ;; TODO: Check bundled sources.
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-crc-3.3.0
  (crate-source "crc" "3.3.0"
                "0xg6yg57lbyzf69y8znq5gjb333w1fnlis2gnjg38blwffrx644p"))

(define rust-crc-3.4.0
  (crate-source "crc" "3.4.0"
                "03dsq5qsv86m35ikg84l80d00wnkjm8q4pjxgac0vaqjrnhs5f2y"))

(define rust-crc-catalog-2.4.0
  (crate-source "crc-catalog" "2.4.0"
                "1xg7sz82w3nxp1jfn425fvn1clvbzb3zgblmxsyqpys0dckp9lqr"))

(define rust-crc32fast-1.4.2
  (crate-source "crc32fast" "1.4.2"
                "1czp7vif73b8xslr3c9yxysmh9ws2r8824qda7j47ffs9pcnjxx9"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-crypto-common-0.1.7
  (crate-source "crypto-common" "0.1.7"
                "02nn2rhfy7kvdkdjl457q2z0mklcvj9h662xrq6dzhfialh2kj3q"))

(define rust-darling-0.20.11
  (crate-source "darling" "0.20.11"
                "1vmlphlrlw4f50z16p4bc9p5qwdni1ba95qmxfrrmzs6dh8lczzw"))

(define rust-darling-core-0.20.11
  (crate-source "darling_core" "0.20.11"
                "0bj1af6xl4ablnqbgn827m43b8fiicgv180749f5cphqdmcvj00d"))

(define rust-darling-macro-0.20.11
  (crate-source "darling_macro" "0.20.11"
                "1bbfbc2px6sj1pqqq97bgqn6c8xdnb2fmz66f7f40nrqrcybjd7w"))

(define rust-deflate64-0.1.10
  (crate-source "deflate64" "0.1.10"
                "012jmx4jrxwdk5d6fbnnxih4zdq9nb0vmjzhqasjkvf5a71qzgr6"))

(define rust-deflate64-0.1.9
  (crate-source "deflate64" "0.1.9"
                "06scix17pa7wzzfsnhkycpcc6s04shs49cdaxx2k1sl0226jnsfs"))

(define rust-deranged-0.4.0
  (crate-source "deranged" "0.4.0"
                "13h6skwk411wzhf1l9l7d3yz5y6vg9d7s3dwhhb4a942r88nm7lw"))

(define rust-deranged-0.5.3
  (crate-source "deranged" "0.5.3"
                "1k473y8lzjac956dm3s1cs2rz364py9zd52y9fkbanws8b6vqc6n"))

(define rust-deranged-0.5.4
  (crate-source "deranged" "0.5.4"
                "0wch36gpg2crz2f72p7c0i5l4bzxjkwxw96sdj57c1cadzw566d4"))

(define rust-deranged-0.5.6
  (crate-source "deranged" "0.5.6"
                "1i48p5l878bw4qzi1wz43lrq3jvplhpdzfxvjg0x3qn2janwagfc"))

(define rust-derive-arbitrary-1.4.1
  (crate-source "derive_arbitrary" "1.4.1"
                "000839h4mbgs65x1f8540kbjk2ifw68c4d8r5b9f7q0jv4d2qm1h"))

(define rust-derive-arbitrary-1.4.2
  (crate-source "derive_arbitrary" "1.4.2"
                "0annkmfwfavd978vwwrxvrpykjfdnc3w6q1ln3j7kyfg5pc7nmhy"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dispatch2-0.2.0
  (crate-source "dispatch2" "0.2.0"
                "186alxavw48mnnyclrhb1w68wsa881cllkp1w227gwiz02g5c38s"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.2
  (crate-source "dlib" "0.5.2"
                "04m4zzybx804394dnqs1blz241xcy480bdwf3w9p4k6c3l46031k"))

(define rust-dns-lookup-2.0.4
  (crate-source "dns-lookup" "2.0.4"
                "1z74n2zij2gahycabm0gkmkyx574h76gwk7sz93yqpr3qa3n0xp5"))

(define rust-dns-lookup-2.1.1
  (crate-source "dns-lookup" "2.1.1"
                "1kwkfxqmyk1dflrhy4yhhz5y9j3b68kcx26gkpy7alpynyj9fmfg"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-endi-1.1.0
  (crate-source "endi" "1.1.0"
                "1gxp388g2zzbncp3rdn60wxkr49xbhhx94nl9p4a6c41w4ma7n53"))

(define rust-enum-ordinalize-4.3.0
  (crate-source "enum-ordinalize" "4.3.0"
                "1max64z9giii61qcwl56rndd7pakaylkaij5zqbbbvjl9vxdr87y"))

(define rust-enum-ordinalize-4.3.2
  (crate-source "enum-ordinalize" "4.3.2"
                "1w0012dqq5y3xikpswix190jvjl097pjrzpi515jr3qzpfkr242a"))

(define rust-enum-ordinalize-derive-4.3.1
  (crate-source "enum-ordinalize-derive" "4.3.1"
                "1zy53fabazimwv5cl0366k834ybixzl84lxj9mfavbnlfn532a0d"))

(define rust-enum-ordinalize-derive-4.3.2
  (crate-source "enum-ordinalize-derive" "4.3.2"
                "0cf61sxxsf3f6n5xhzjxdrc1793k52250ql32zp9h9fnn8gn1acc"))

(define rust-enumflags2-0.7.12
  (crate-source "enumflags2" "0.7.12"
                "1vzcskg4dca2jiflsfx1p9yw1fvgzcakcs7cpip0agl51ilgf9qh"))

(define rust-enumflags2-derive-0.7.12
  (crate-source "enumflags2_derive" "0.7.12"
                "09rqffacafl1b83ir55hrah9gza0x7pzjn6lr6jm76fzix6qmiv7"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.13
  (crate-source "errno" "0.3.13"
                "1bd5g3srn66zr3bspac0150bvpg1s7zi6zwhwhlayivciz12m3kp"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-event-listener-5.4.0
  (crate-source "event-listener" "5.4.0"
                "1bii2gn3vaa33s0gr2zph7cagiq0ppcfxcxabs24ri9z9kgar4il"))

(define rust-event-listener-5.4.1
  (crate-source "event-listener" "5.4.1"
                "1asnp3agbr8shcl001yd935m167ammyi8hnvl0q1ycajryn6cfz1"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-filetime-0.2.25
  (crate-source "filetime" "0.2.25"
                "11l5zr86n5sr6g6k6sqldswk0jzklm0q95rzikxcns0yk0p55h1m"))

(define rust-filetime-0.2.26
  (crate-source "filetime" "0.2.26"
                "1vb3vz83saxr084wjf2032hspx7wfc5ggggnhc15i9kg3g6ha1dw"))

(define rust-filetime-0.2.27
  (crate-source "filetime" "0.2.27"
                "1nspbkm1d1km7xfljcbl565swqxrihqyin8bqppig2gf3qal927r"))

(define rust-find-msvc-tools-0.1.2
  (crate-source "find-msvc-tools" "0.1.2"
                "0nbrhvk4m04hviiwbqp2jwcv9j2k70x0q2kcvfk51iygvaqp7v8w"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-fluent-bundle-0.15.3
  (crate-source "fluent-bundle" "0.15.3"
                "14zl0cjn361is69pb1zry4k2zzh5nzsfv0iz05wccl00x0ga5q3z"))

(define rust-fluent-bundle-0.16.0
  (crate-source "fluent-bundle" "0.16.0"
                "1x1v8bmym6x9pl87f82lbzwlc84kdn0lgcwi73ki2mwgj6w3q801"))

(define rust-fluent-langneg-0.13.0
  (crate-source "fluent-langneg" "0.13.0"
                "152yxplc11vmxkslvmaqak9x86xnavnhdqyhrh38ym37jscd0jic"))

(define rust-fluent-syntax-0.11.1
  (crate-source "fluent-syntax" "0.11.1"
                "0gd3cdvsx9ymbb8hijcsc9wyf8h1pbcbpsafg4ldba56ji30qlra"))

(define rust-fluent-syntax-0.12.0
  (crate-source "fluent-syntax" "0.12.0"
                "1661sp6kl268n445x7jjhnbkgiaa1xcpyryq0i6iiz9zqn3x5w2l"))

(define rust-fluent-template-macros-0.11.0
  (crate-source "fluent-template-macros" "0.11.0"
                "02pj22rv9j0bv9wcwcfsivywzznc56m05pr58anp6kwaarp1fz80"))

(define rust-fluent-template-macros-0.13.0
  (crate-source "fluent-template-macros" "0.13.0"
                "188r3cxd9yk27g2zbbvksyc2p3xbbpdi6pnb0pfaq38xc14j9l2f"))

(define rust-fluent-template-macros-0.13.1
  (crate-source "fluent-template-macros" "0.13.1"
                "1vhpfq7kww8wrccb2w8m8vj79k5ha5k638sdk1xhx7wb4252n8p6"))

(define rust-fluent-templates-0.11.0
  (crate-source "fluent-templates" "0.11.0"
                "1b5p5fvgdz2bgkjcc0cl105kdrc42hw6mh8kx9ii2mf8n9hjzwkl"))

(define rust-fluent-templates-0.13.0
  (crate-source "fluent-templates" "0.13.0"
                "11jkx2rhqxgclbijiy86217rb5zsmmrfr6m7zig4jabnx1gmm1b9"))

(define rust-fluent-templates-0.13.1
  (crate-source "fluent-templates" "0.13.1"
                "0fsi1yyvwbzcdi80qlnlhp5jpq42bnx9q7j2g0zxqj0fgkbr7a7q"))

(define rust-flume-0.11.1
  (crate-source "flume" "0.11.1"
                "15ch0slxa8sqsi6c73a0ky6vdnh48q8cxjf7rksa3243m394s3ns"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-form-urlencoded-1.2.1
  (crate-source "form_urlencoded" "1.2.1"
                "0milh8x7nl4f450s3ddhg57a3flcv6yq8hlkyk6fyr3mcb128dp1"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fragile-2.0.1
  (crate-source "fragile" "2.0.1"
                "06g69s9w3hmdnjp5b60ph15v367278mgxy1shijrllarc2pnrp98"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-futures-0.3.31
  (crate-source "futures" "0.3.31"
                "0xh8ddbkm9jy8kc5gbvjp9a4b6rqqxvc8471yb2qaz5wm2qhgg35"))

(define rust-futures-channel-0.3.31
  (crate-source "futures-channel" "0.3.31"
                "040vpqpqlbk099razq8lyn74m0f161zd0rp36hciqrwcg2zibzrd"))

(define rust-futures-core-0.3.31
  (crate-source "futures-core" "0.3.31"
                "0gk6yrxgi5ihfanm2y431jadrll00n5ifhnpx090c2f2q1cr1wh5"))

(define rust-futures-executor-0.3.31
  (crate-source "futures-executor" "0.3.31"
                "17vcci6mdfzx4gbk0wx64chr2f13wwwpvyf3xd5fb1gmjzcx2a0y"))

(define rust-futures-io-0.3.31
  (crate-source "futures-io" "0.3.31"
                "1ikmw1yfbgvsychmsihdkwa8a1knank2d9a8dk01mbjar9w1np4y"))

(define rust-futures-lite-2.6.0
  (crate-source "futures-lite" "2.6.0"
                "0cmmgszlmkwsac9pyw5rfjakmshgx4wmzmlyn6mmjs0jav4axvgm"))

(define rust-futures-lite-2.6.1
  (crate-source "futures-lite" "2.6.1"
                "1ba4dg26sc168vf60b1a23dv1d8rcf3v3ykz2psb7q70kxh113pp"))

(define rust-futures-macro-0.3.31
  (crate-source "futures-macro" "0.3.31"
                "0l1n7kqzwwmgiznn0ywdc5i24z72zvh9q1dwps54mimppi7f6bhn"))

(define rust-futures-sink-0.3.31
  (crate-source "futures-sink" "0.3.31"
                "1xyly6naq6aqm52d5rh236snm08kw8zadydwqz8bip70s6vzlxg5"))

(define rust-futures-task-0.3.31
  (crate-source "futures-task" "0.3.31"
                "124rv4n90f5xwfsm9qw6y99755y021cmi5dhzh253s920z77s3zr"))

(define rust-futures-util-0.3.31
  (crate-source "futures-util" "0.3.31"
                "10aa1ar8bgkgbr4wzxlidkqkcxf77gffyj8j7768h831pcaq784z"))

(define rust-gdk-pixbuf-0.20.10
  (crate-source "gdk-pixbuf" "0.20.10"
                "0371cfhxldrn2pf8zdjyx2b3xkhbfm96k988spp4nkq89j4l5lig"))

(define rust-gdk-pixbuf-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "gdk-pixbuf-sys" "0.20.10"
                "15hb2f5mmyg5amaha6lx6spaygw2b7ga4hwmgqhvv269h2sz6d2v"))

(define rust-gdk4-0.9.6
  (crate-source "gdk4" "0.9.6"
                "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28"))

(define rust-gdk4-sys-0.9.6
  ;; TODO: Check bundled sources.
  (crate-source "gdk4-sys" "0.9.6"
                "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.3
  (crate-source "getrandom" "0.3.3"
                "1x6jl875zp6b2b6qp9ghc84b0l76bvng2lvm8zfcmwjl7rb5w516"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.1
  (crate-source "getrandom" "0.4.1"
                "1v7fm84f2jh6x7w3bd2ncl3sw29wnb0rhg7xya1pd30i02cg77hk"))

(define rust-gimli-0.31.1
  (crate-source "gimli" "0.31.1"
                "0gvqc0ramx8szv76jhfd4dms0zyamvlg4whhiz11j34hh3dqxqh7"))

(define rust-gimli-0.32.3
  (crate-source "gimli" "0.32.3"
                "1iqk5xznimn5bfa8jy4h7pa1dv3c624hzgd2dkz8mpgkiswvjag6"))

(define rust-gio-0.20.12
  (crate-source "gio" "0.20.12"
                "0cdq5116cwdgs0xkdp1v146yhcqilxlpgvkncc7xbf5nwxvf49wf"))

(define rust-gio-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "gio-sys" "0.20.10"
                "10vc6gqhz5crnrh040rv6r5nm09njky2r9d9ms29xj3gwnkr67jj"))

(define rust-glib-0.20.12
  (crate-source "glib" "0.20.12"
                "10ynn8aiabbzrsgdswmqpr47sapfkbfn5rfxsy26swflabivdi7z"))

(define rust-glib-build-tools-0.20.0
  (crate-source "glib-build-tools" "0.20.0"
                "1n1n8w9ls1nr582gd6yfd4x6sp36jlcqmv4kx8z5lpcv3mjw4abh"))

(define rust-glib-macros-0.20.12
  (crate-source "glib-macros" "0.20.12"
                "0ibi9vbpbw9vvl9ax60kxq07d7a21k0jj5lva8zmliq95zv4l278"))

(define rust-glib-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "glib-sys" "0.20.10"
                "05f29ky5dnvy8vp5rdld5f8r2lgr5w7dxqr7p27km016s4g9xdwa"))

(define rust-globset-0.4.16
  (crate-source "globset" "0.4.16"
                "1xa9ivqs74imf1q288spxh49g6iw2mn3x9snibdgapazzj6h58al"))

(define rust-gobject-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "gobject-sys" "0.20.10"
                "1niyqv22b2c38ks33i4isas4v83d3w7jx3xzzly9x63kpfacm6pc"))

(define rust-graphene-rs-0.20.10
  (crate-source "graphene-rs" "0.20.10"
                "16six67j0j57ynv7frxiwnsf7dslhyy67ppirad1q98lgnnxz1kb"))

(define rust-graphene-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "graphene-sys" "0.20.10"
                "1sk1736b4vay2hj9qz56c0pvqa3v0mkdch3yg7hiapidpa2kln6z"))

(define rust-gsk4-0.9.6
  (crate-source "gsk4" "0.9.6"
                "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1"))

(define rust-gsk4-sys-0.9.6
  ;; TODO: Check bundled sources.
  (crate-source "gsk4-sys" "0.9.6"
                "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m"))

(define rust-gtk4-0.9.7
  (crate-source "gtk4" "0.9.7"
                "1mi6lcwm25jz7lznrb9glaabgyk40hnvkg4fzaxlf762080xsx7j"))

(define rust-gtk4-macros-0.9.5
  (crate-source "gtk4-macros" "0.9.5"
                "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f"))

(define rust-gtk4-sys-0.9.6
  ;; TODO: Check bundled sources.
  (crate-source "gtk4-sys" "0.9.6"
                "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21"))

(define rust-h2-0.4.12
  (crate-source "h2" "0.4.12"
                "11hk5mpid8757z6n3v18jwb62ikffrgzjlrgpzqvkqdlzjfbdh7k"))

(define rust-h2-0.4.13
  (crate-source "h2" "0.4.13"
                "0m6w5gg0n0m1m5915bxrv8n4rlazhx5icknkslz719jhh4xdli1g"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.4
  (crate-source "hashbrown" "0.15.4"
                "1mg045sm1nm00cwjm7ndi80hcmmv1v3z7gnapxyhd9qxc62sqwar"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.16.0
  (crate-source "hashbrown" "0.16.0"
                "13blh9j2yv77a6ni236ixiwdzbc1sh2bc4bdpaz7y859yv2bs6al"))

(define rust-hashbrown-0.16.1
  (crate-source "hashbrown" "0.16.1"
                "004i3njw38ji3bzdp9z178ba9x3k0c1pgy8x69pj7yfppv4iq7c4"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.11
  (crate-source "home" "0.5.11"
                "1kxb4k87a9sayr8jipr7nq9wpgmjk4hk4047hmf9kc24692k75aq"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

(define rust-http-1.3.1
  (crate-source "http" "1.3.1"
                "0r95i5h7dr1xadp1ac9453w0s62s27hzkam356nyx2d9mqqmva7l"))

(define rust-http-1.4.0
  (crate-source "http" "1.4.0"
                "06iind4cwsj1d6q8c2xgq8i2wka4ps74kmws24gsi1bzdlw2mfp3"))

(define rust-http-body-1.0.1
  (crate-source "http-body" "1.0.1"
                "111ir5k2b9ihz5nr9cz7cwm7fnydca7dx4hc7vr16scfzghxrzhy"))

(define rust-http-body-util-0.1.3
  (crate-source "http-body-util" "0.1.3"
                "0jm6jv4gxsnlsi1kzdyffjrj8cfr3zninnxpw73mvkxy4qzdj8dh"))

(define rust-httparse-1.10.1
  (crate-source "httparse" "1.10.1"
                "11ycd554bw2dkgw0q61xsa7a4jn1wb1xbfacmf3dbwsikvkkvgvd"))

(define rust-human-panic-2.0.3
  (crate-source "human-panic" "2.0.3"
                "02ph7nnn5wm60p2fd65gyq1z5zhw9nq0x18nzr8mvsc7n53afqxc"))

(define rust-hyper-1.7.0
  (crate-source "hyper" "1.7.0"
                "07n59pxzlq621z611cbpvh7p4h9h15v0r7m5wgxygpx02d5aafpb"))

(define rust-hyper-1.8.1
  (crate-source "hyper" "1.8.1"
                "04cxr8j5y86bhxxlyqb8xkxjskpajk7cxwfzzk4v3my3a3rd9cia"))

(define rust-hyper-rustls-0.27.7
  (crate-source "hyper-rustls" "0.27.7"
                "0n6g8998szbzhnvcs1b7ibn745grxiqmlpg53xz206v826v3xjg3"))

(define rust-hyper-util-0.1.16
  (crate-source "hyper-util" "0.1.16"
                "0pmw8gqkqjnsdrxdy5wd5q8z1gh7caxqk2an7b4s53byghkhb6wd"))

(define rust-hyper-util-0.1.19
  (crate-source "hyper-util" "0.1.19"
                "0pyzc8378baf996l5ycl4y0s3skhxc4z4vkah9mvff3r1vb0ay3j"))

(define rust-hyper-util-0.1.20
  (crate-source "hyper-util" "0.1.20"
                "186zdc58hmm663csmjvrzgkr6jdh93sfmi3q2pxi57gcaqjpqm4n"))

(define rust-icu-collections-2.0.0
  (crate-source "icu_collections" "2.0.0"
                "0izfgypv1hsxlz1h8fc2aak641iyvkak16aaz5b4aqg3s3sp4010"))

(define rust-icu-collections-2.1.1
  (crate-source "icu_collections" "2.1.1"
                "0hsblchsdl64q21qwrs4hvc2672jrf466zivbj1bwyv606bn8ssc"))

(define rust-icu-locale-core-2.0.0
  (crate-source "icu_locale_core" "2.0.0"
                "02phv7vwhyx6vmaqgwkh2p4kc2kciykv2px6g4h8glxfrh02gphc"))

(define rust-icu-locale-core-2.1.1
  (crate-source "icu_locale_core" "2.1.1"
                "1djvdc2f5ylmp1ymzv4gcnmq1s4hqfim9nxlcm173lsd01hpifpd"))

(define rust-icu-normalizer-2.0.0
  (crate-source "icu_normalizer" "2.0.0"
                "0ybrnfnxx4sf09gsrxri8p48qifn54il6n3dq2xxgx4dw7l80s23"))

(define rust-icu-normalizer-2.1.1
  (crate-source "icu_normalizer" "2.1.1"
                "16dmn5596la2qm0r3vih0bzjfi0vx9a20yqjha6r1y3vnql8hv2z"))

(define rust-icu-normalizer-data-2.0.0
  (crate-source "icu_normalizer_data" "2.0.0"
                "1lvjpzxndyhhjyzd1f6vi961gvzhj244nribfpdqxjdgjdl0s880"))

(define rust-icu-normalizer-data-2.1.1
  (crate-source "icu_normalizer_data" "2.1.1"
                "02jnzizg6q75m41l6c13xc7nkc5q8yr1b728dcgfhpzw076wrvbs"))

(define rust-icu-properties-2.0.1
  (crate-source "icu_properties" "2.0.1"
                "0az349pjg8f18lrjbdmxcpg676a7iz2ibc09d2wfz57b3sf62v01"))

(define rust-icu-properties-2.1.2
  (crate-source "icu_properties" "2.1.2"
                "1v3lbmhhi7i6jgw51ikjb1p50qh5rb67grlkdnkc63l7zq1gq2q2"))

(define rust-icu-properties-data-2.0.1
  (crate-source "icu_properties_data" "2.0.1"
                "0cnn3fkq6k88w7p86w7hsd1254s4sl783rpz4p6hlccq74a5k119"))

(define rust-icu-properties-data-2.1.2
  (crate-source "icu_properties_data" "2.1.2"
                "1bvpkh939rgzrjfdb7hz47v4wijngk0snmcgrnpwc9fpz162jv31"))

(define rust-icu-provider-2.0.0
  (crate-source "icu_provider" "2.0.0"
                "1bz5v02gxv1i06yhdhs2kbwxkw3ny9r2vvj9j288fhazgfi0vj03"))

(define rust-icu-provider-2.1.1
  (crate-source "icu_provider" "2.1.1"
                "0576b7dizgyhpfa74kacv86y4g1p7v5ffd6c56kf1q82rvq2r5l5"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.0.3
  (crate-source "idna" "1.0.3"
                "0zlajvm2k3wy0ay8plr07w22hxkkmrxkffa6ah57ac6nci984vv8"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.23
  (crate-source "ignore" "0.4.23"
                "0jysggjfmlxbg60vhhiz4pb8jfb7cnq5swdsvxknbs7x18wgv2bd"))

(define rust-indexmap-2.10.0
  (crate-source "indexmap" "2.10.0"
                "0qd6g26gxzl6dbf132w48fa8rr95glly3jhbk90i29726d9xhk7y"))

(define rust-indexmap-2.11.4
  (crate-source "indexmap" "2.11.4"
                "1rc8bgcjzfcskz1zipjjm7s3m1jskzhnhr9jxmsafhdk1xv863sb"))

(define rust-indexmap-2.13.0
  (crate-source "indexmap" "2.13.0"
                "05qh5c4h2hrnyypphxpwflk45syqbzvqsvvyxg43mp576w2ff53p"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-intl-memoizer-0.5.3
  (crate-source "intl-memoizer" "0.5.3"
                "0gqn5wwhzacvj0z25r5r3l2pajg9c8i1ivh7g8g8dszm8pis439i"))

(define rust-intl-pluralrules-7.0.2
  (crate-source "intl_pluralrules" "7.0.2"
                "0wprd3h6h8nfj62d8xk71h178q7zfn3srxm787w4sawsqavsg3h7"))

(define rust-io-uring-0.7.10
  (crate-source "io-uring" "0.7.10"
                "0yvjyygwdcqjcgw8zp254hvjbm7as1c075dl50spdshas3aa4vq4"))

(define rust-io-uring-0.7.8
  (crate-source "io-uring" "0.7.8"
                "04whnj5a4pml44jhsmmf4p87bpgr7swkcijx4yjcng8900pj0vmq"))

(define rust-ipnet-2.11.0
  (crate-source "ipnet" "2.11.0"
                "0c5i9sfi2asai28m8xp48k5gvwkqrg5ffpi767py6mzsrswv17s6"))

(define rust-iri-string-0.7.10
  (crate-source "iri-string" "0.7.10"
                "06kk3a5jz576p7vrpf7zz9jv3lrgcyp7pczcblcxdnryg3q3h4y9"))

(define rust-iri-string-0.7.8
  (crate-source "iri-string" "0.7.8"
                "1cl0wfq97wq4s1p4dl0ix5cfgsc5fn7l22ljgw9ab9x1qglypifv"))

(define rust-iri-string-0.7.9
  (crate-source "iri-string" "0.7.9"
                "15s3s6k99ci52d7qdplhllpa6xyvdyiys645n6z6fsw93nfpp1jg"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-itoa-1.0.17
  (crate-source "itoa" "1.0.17"
                "1lh93xydrdn1g9x547bd05g0d3hra7pd1k4jfd2z1pl1h5hwdv4j"))

(define rust-jobserver-0.1.33
  (crate-source "jobserver" "0.1.33"
                "12jkn3cxvfs7jsb6knmh9y2b41lwmrk3vdqywkmssx61jzq65wiq"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.77"
                "13x2qcky5l22z4xgivi59xhjjx4kxir1zg7gcj0f1ijzd4yg7yhw"))

(define rust-js-sys-0.3.80
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.80"
                "0bkhnbna0a9sqhhswfar0mzi8mpy2dygv4zbzfdbm97bqnz16bw5"))

(define rust-js-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "js-sys" "0.3.81"
                "01ckbf16iwh7qj92fax9zh8vf2y9sk60cli6999cn7a1jxx96j7c"))

(define rust-js-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.85"
                "1csmb42fxjmzjdgc790bgw77sf1cb9ydm5rdsnh5qj4miszjx54c"))

(define rust-kinda-virtual-fs-0.1.1
  (crate-source "kinda-virtual-fs" "0.1.1"
                "17kccqzhkdffwgg7y8l8jr9ykwh95777gm151xjva5vrq6m2b4ya"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-libadwaita-0.7.2
  (crate-source "libadwaita" "0.7.2"
                "14c1qy6mq5l9wlwsr2x9ijbvis283msfglxgp9kvzahnkk93a0ah"))

(define rust-libadwaita-sys-0.7.2
  ;; TODO: Check bundled sources.
  (crate-source "libadwaita-sys" "0.7.2"
                "1nqjr514hhdc4aldlsc4y3vkpnkq9q73g2jl7ypqnmf2b209i036"))

(define rust-libc-0.2.174
  (crate-source "libc" "0.2.174"
                "0xl7pqvw7g2874dy3kjady2fjr4rhj5lxsnxkkhr5689jcr6jw8i"))

(define rust-libc-0.2.175
  (crate-source "libc" "0.2.175"
                "0hw5sb3gjr0ivah7s3fmavlpvspjpd4mr009abmam2sr7r4sx0ka"))

(define rust-libc-0.2.176
  (crate-source "libc" "0.2.176"
                "0x7ivn80h7nz2l46vra7bxx36s6r8d0lkax14dx97skjsss2kyaq"))

(define rust-libc-0.2.182
  (crate-source "libc" "0.2.182"
                "04k1w1mq9f4cxv520dbr5xw1i7xkbc9fcrvaggyjy25jdkdvl038"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libredox-0.1.12
  (crate-source "libredox" "0.1.12"
                "05h6fb2y05h74zwaafmnf7gv3bxilzp7syqlfzw524w55kh9a2rx"))

(define rust-libredox-0.1.4
  (crate-source "libredox" "0.1.4"
                "0f06ikfym363zrqy9llp4asgcbakz0aiq0ds0rkljdg52088100m"))

(define rust-libz-rs-sys-0.5.1
  ;; TODO: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.1"
                "08a2grn3bp05696pd27s6kmq1icnbzffizl0nihic8m26y2phahp"))

(define rust-libz-rs-sys-0.5.2
  ;; TODO: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.2"
                "1kdy093bhxfkgx7li3raxigcc3qdqjn3hvrpjkblvv6r777vh3c4"))

(define rust-linux-raw-sys-0.11.0
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.11.0"
                "0fghx0nn8nvbz5yzgizfcwd6ap2pislp68j8c1bwyr6sacxkq7fz"))

(define rust-linux-raw-sys-0.4.15
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-linux-raw-sys-0.9.4
  ;; TODO: Check bundled sources.
  (crate-source "linux-raw-sys" "0.9.4"
                "04kyjdrq79lz9ibrf7czk6cv9d3jl597pb9738vzbsbzy1j5i56d"))

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-litemap-0.8.1
  (crate-source "litemap" "0.8.1"
                "0xsy8pfp9s802rsj1bq2ys2kbk1g36w5dr3gkfip7gphb5x60wv3"))

(define rust-lock-api-0.4.13
  (crate-source "lock_api" "0.4.13"
                "0rd73p4299mjwl4hhlfj9qr88v3r0kc8s1nszkfmnq2ky43nb4wn"))

(define rust-log-0.4.27
  (crate-source "log" "0.4.27"
                "150x589dqil307rv0rwj0jsgz5bjbwvl83gyl61jf873a7rjvp0k"))

(define rust-log-0.4.28
  (crate-source "log" "0.4.28"
                "0cklpzrpxafbaq1nyxarhnmcw9z3xcjrad3ch55mmr58xw2ha21l"))

(define rust-log-0.4.29
  (crate-source "log" "0.4.29"
                "15q8j9c8g5zpkcw0hnd6cf2z7fxqnvsjh3rw5mv5q10r83i34l2y"))

(define rust-lru-slab-0.1.2
  (crate-source "lru-slab" "0.1.2"
                "0m2139k466qj3bnpk66bwivgcx3z88qkxvlzk70vd65jq373jaqi"))

(define rust-lzma-rs-0.3.0
  (crate-source "lzma-rs" "0.3.0"
                "0phif4pnjrn28zcxgz3a7z86hhx5gdajmkrndfw4vrkahd682zi9"))

(define rust-lzma-sys-0.1.20
  ;; TODO: Check bundled sources.
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-md5-asm-0.5.2
  (crate-source "md5-asm" "0.5.2"
                "1pz217kwlvrw4bj4hil5acyp3l7g37vwf25psdc210bxzkkqx6yi"))

(define rust-memchr-2.7.5
  (crate-source "memchr" "2.7.5"
                "1h2bh2jajkizz04fh047lpid5wgw2cr9igpkdhl3ibzscpd858ij"))

(define rust-memchr-2.7.6
  (crate-source "memchr" "2.7.6"
                "0wy29kf6pb4fbhfksjbs05jy2f32r2f3r1ga6qkmpz31k79h0azm"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-minreq-2.14.0
  (crate-source "minreq" "2.14.0"
                "0y5nw39r9w8kjcj86k17wc3q8s9z9bjs216blgsbycb8m0957244"))

(define rust-minreq-2.14.1
  (crate-source "minreq" "2.14.1"
                "0g8mw9fncqngxrwkvyxfcid6h1lhknfpwd6aj4bddxyhv8152085"))

(define rust-mio-1.0.4
  (crate-source "mio" "1.0.4"
                "073n3kam3nz8j8had35fd2nn7j6a33pi3y5w3kq608cari2d9gkq"))

(define rust-mio-1.1.1
  (crate-source "mio" "1.1.1"
                "1z2phpalqbdgihrcjp8y09l3kgq6309jnhnr6h11l9s7mnqcm6x6"))

(define rust-nanorand-0.7.0
  (crate-source "nanorand" "0.7.0"
                "1hr60b8zlfy7mxjcwx2wfmhpkx7vfr3v9x12shmv1c10b0y32lba"))

(define rust-nix-0.30.1
  (crate-source "nix" "0.30.1"
                "1dixahq9hk191g0c2ydc0h1ppxj0xw536y6rl63vlnp06lx3ylkl"))

(define rust-ntapi-0.4.1
  (crate-source "ntapi" "0.4.1"
                "1r38zhbwdvkis2mzs6671cm1p6djgsl49i7bwxzrvhwicdf8k8z8"))

(define rust-ntapi-0.4.3
  (crate-source "ntapi" "0.4.3"
                "1bl0d73avwla7laa4pkqvzvifjbs0avg65w01zxjydgx3likbcy3"))

(define rust-nu-ansi-term-0.46.0
  (crate-source "nu-ansi-term" "0.46.0"
                "115sywxh53p190lyw97alm14nc004qj5jm5lvdj608z84rbida3p"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-num-conv-0.1.0
  (crate-source "num-conv" "0.1.0"
                "1ndiyg82q73783jq18isi71a7mjh56wxrk52rlvyx0mi5z9ibmai"))

(define rust-num-conv-0.2.0
  (crate-source "num-conv" "0.2.0"
                "0l4hj7lp8zbb9am4j3p7vlcv47y9bbazinvnxx9zjhiwkibyr5yg"))

(define rust-objc2-0.6.1
  (crate-source "objc2" "0.6.1"
                "0l85a8r77i8i183fqyx55kqm2nh9rzg2z3z59kjb4fj92iz5kil8"))

(define rust-objc2-0.6.2
  (crate-source "objc2" "0.6.2"
                "1g3qa1vxp6nlh4wllll921z299d3s1is31m1ccasd8pklxxka7sn"))

(define rust-objc2-app-kit-0.3.1
  (crate-source "objc2-app-kit" "0.3.1"
                "1k4vz0s63rpp1yyhx96mh9nndn1zzv2cwxzpvw6rnigcidb9zwp6"))

(define rust-objc2-core-foundation-0.3.1
  (crate-source "objc2-core-foundation" "0.3.1"
                "0rn19d70mwxyv74kx7aqm5in6x320vavq9v0vrm81vbg9a4w440w"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.1
  (crate-source "objc2-foundation" "0.3.1"
                "0g5hl47dxzabs7wndcg6kz3q137v9hwfay1jd2da1q9gglj3224h"))

(define rust-objc2-io-kit-0.3.1
  (crate-source "objc2-io-kit" "0.3.1"
                "02iwv7pppxvna72xwd7y5q67hrnbn5v73xikc3c1rr90c56wdhbi"))

(define rust-objc2-io-kit-0.3.2
  (crate-source "objc2-io-kit" "0.3.2"
                "05dvfcf97w39daaj5qsbfc399lw9hbx3s4h9nwgxrmlpjnizpyik"))

(define rust-object-0.36.7
  (crate-source "object" "0.36.7"
                "11vv97djn9nc5n6w1gc6bd96d2qk2c8cg1kw5km9bsi3v4a8x532"))

(define rust-object-0.37.3
  (crate-source "object" "0.37.3"
                "1zikiy9xhk6lfx1dn2gn2pxbnfpmlkn0byd7ib1n720x0cgj0xpz"))

(define rust-once-cell-1.21.3
  (crate-source "once_cell" "1.21.3"
                "0b9x77lb9f1j6nqgf5aka4s2qj0nly176bpbrv6f9iakk5ff3xa2"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-open-5.3.2
  (crate-source "open" "5.3.2"
                "15ggfx1p8rl7w4rr1n5qj1wxy1kk7757lsjpyc947a9fwri3aj72"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-os-info-3.12.0
  (crate-source "os_info" "3.12.0"
                "1hzzmxj8z69q5l1hzlnqnaa56ip9kvmghp8k750w6hwdvrgsrqfh"))

(define rust-overload-0.1.1
  (crate-source "overload" "0.1.1"
                "0fdgbaqwknillagy1xq7xfgv60qdbk010diwl7s1p0qx7hb16n5i"))

(define rust-pango-0.20.12
  (crate-source "pango" "0.20.12"
                "0p5bj7k8sd2pgm7v907i9bip53ys46hahprs0jbr6rfzyq8v6xk5"))

(define rust-pango-sys-0.20.10
  ;; TODO: Check bundled sources.
  (crate-source "pango-sys" "0.20.10"
                "1yj3n87whqx6gw3vip08zbckqxfg7l5jqc2wamaf76y07xkhjs8q"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-percent-encoding-2.3.1
  (crate-source "percent-encoding" "2.3.1"
                "0gi8wgx0dcy8rnv1kywdv98lwcx67hz0a0zwpib5v2i08r88y573"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pin-utils-0.1.0
  (crate-source "pin-utils" "0.1.0"
                "117ir7vslsl2z1a7qzhws4pd01cg2d3338c47swjyvqv2n60v1wb"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-plist-1.7.4
  (crate-source "plist" "1.7.4"
                "1qg7zkvnm4r4n9s9hbwwjnwznvkg6v0f035hza4agib3w64vbxis"))

(define rust-plist-1.8.0
  (crate-source "plist" "1.8.0"
                "01qyv51ljbvhjbg8mva5c802b3dzrr95y6nd23wjh52xbjhvw3kl"))

(define rust-pollster-0.4.0
  (crate-source "pollster" "0.4.0"
                "1qqcn0h2bvmgm9rlhfrdk7lfaiw1ad86g9500bhx1rj1s0c9yfig"))

(define rust-potential-utf-0.1.2
  (crate-source "potential_utf" "0.1.2"
                "11dm6k3krx3drbvhgjw8z508giiv0m09wzl6ghza37176w4c79z5"))

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-potential-utf-0.1.4
  (crate-source "potential_utf" "0.1.4"
                "0xxg0pkfpq299wvwln409z4fk80rbv55phh3f1jhjajy5x1ljfdp"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.3.0
  (crate-source "proc-macro-crate" "3.3.0"
                "0d9xlymplfi9yv3f5g4bp0d6qh70apnihvqcjllampx4f5lmikpd"))

(define rust-proc-macro-crate-3.4.0
  (crate-source "proc-macro-crate" "3.4.0"
                "10v9qi51n4phn1lrj5r94kjq7yhci9jrkqnn6wpan05yjsgb3711"))

(define rust-proc-macro-hack-0.5.20+deprecated
  (crate-source "proc-macro-hack" "0.5.20+deprecated"
                "0s402hmcs3k9nd6rlp07zkr1lz7yimkmcwcbgnly2zr44wamwdyw"))

(define rust-proc-macro2-1.0.101
  (crate-source "proc-macro2" "1.0.101"
                "1pijhychkpl7rcyf1h7mfk6gjfii1ywf5n0snmnqs5g4hvyl7bl9"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

(define rust-proc-macro2-1.0.95
  (crate-source "proc-macro2" "1.0.95"
                "0y7pwxv6sh4fgg6s715ygk1i7g3w02c0ljgcsfm046isibkfbcq2"))

(define rust-protobuf-3.7.2
  (crate-source "protobuf" "3.7.2"
                "1x4riz4znnjsqpdxnhxj0aq8rfivmbv4hfqmd3gbbn77v96isnnn"))

(define rust-protobuf-codegen-3.7.2
  (crate-source "protobuf-codegen" "3.7.2"
                "1kjaakqk0595akxdhv68w23zw136hw0h0kxkyg9bn500bj17cfax"))

(define rust-protobuf-parse-3.7.2
  (crate-source "protobuf-parse" "3.7.2"
                "0wy9pnfrsk2iz2ghhvzdpp0riklrm6p8dvdfxr4d7wb04hgsmbml"))

(define rust-protobuf-support-3.7.2
  (crate-source "protobuf-support" "3.7.2"
                "1mnpn2q96bxm2vidh86m5p2x5z0z8rgfyixk1wlgjiqa3vrw4diy"))

(define rust-quick-xml-0.37.5
  (crate-source "quick-xml" "0.37.5"
                "1yxpd7rc2qn6f4agfj47ps2z89vv7lvzxpzawqirix8bmyhrf7ik"))

(define rust-quick-xml-0.38.0
  (crate-source "quick-xml" "0.38.0"
                "06vvgd9arm1nrsd4d0ii6lhnp6m11bwy7drqa4k9hnjw9xkb09w9"))

(define rust-quick-xml-0.38.3
  (crate-source "quick-xml" "0.38.3"
                "12bvsbnnmlnq9xg9in3h3080ag3sisafgpcn7lqyzhkz93kk58j2"))

(define rust-quinn-0.11.8
  (crate-source "quinn" "0.11.8"
                "1j02h87nfxww5mjcw4vjcnx8b70q0yinnc8xvjv82ryskii18qk2"))

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.12
  (crate-source "quinn-proto" "0.11.12"
                "0bj2yyrf1mrg2bcj19ipsspvrj5sq0di0pz5maw5pj31j4x89ps9"))

(define rust-quinn-proto-0.11.13
  (crate-source "quinn-proto" "0.11.13"
                "0cca3mgja9p4w66f6sl1kfhj8rdf4mwsg1jxzssh9g63n14np47i"))

(define rust-quinn-udp-0.5.13
  (crate-source "quinn-udp" "0.5.13"
                "0w0ri3wv5g419i5dfv4qmjxh4ayc4hp77y2gy4p3axp2kqhb3szw"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quote-1.0.40
  (crate-source "quote" "1.0.40"
                "1394cxjg6nwld82pzp2d4fp6pmaz32gai1zh9z5hvh0dawww118q"))

(define rust-quote-1.0.44
  (crate-source "quote" "1.0.44"
                "1r7c7hxl66vz3q9qizgjhy77pdrrypqgk4ghc7260xvvfb7ypci1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-rand-0.9.1
  (crate-source "rand" "0.9.1"
                "15yxfcxbgmwba5cv7mjg9bhc1r5c9483dfcdfspg62x4jk8dkgwz"))

(define rust-rand-0.9.2
  (crate-source "rand" "0.9.2"
                "1lah73ainvrgl7brcxx0pwhpnqa3sm3qaj672034jz8i0q7pgckd"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.9.3
  (crate-source "rand_core" "0.9.3"
                "0f3xhf16yks5ic6kmgxcpv1ngdhp48mmfy4ag82i1wnwh8ws3ncr"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-redox-syscall-0.5.13
  (crate-source "redox_syscall" "0.5.13"
                "1mlzna9bcd7ss1973bmysr3hpjrys82b3bd7l03h4jkbxv8bf10d"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-syscall-0.7.1
  (crate-source "redox_syscall" "0.7.1"
                "0axziqgk7mg2cwmr03qq250z24gxhqn02cn29zi05p5d22k5m61m"))

(define rust-regex-1.11.1
  (crate-source "regex" "1.11.1"
                "148i41mzbx8bmq32hsj1q4karkzzx5m60qza6gdw4pdc9qdyyi5m"))

(define rust-regex-1.11.3
  (crate-source "regex" "1.11.3"
                "0b58ya98c4i5cjjiwhpcnjr61cv9g143qhdwhsryggj09098hllb"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.4.10
  (crate-source "regex-automata" "0.4.10"
                "1mllcfmgjcl6d52d5k09lwwq9wj5mwxccix4bhmw5spy1gx5i53b"))

(define rust-regex-automata-0.4.11
  (crate-source "regex-automata" "0.4.11"
                "1bawj908pxixpggcnma3xazw53mwyz68lv9hn4yg63nlhv7bjgl3"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-automata-0.4.9
  (crate-source "regex-automata" "0.4.9"
                "02092l8zfh3vkmk47yjc8d631zhhcd49ck2zr133prvd3z38v7l0"))

(define rust-regex-syntax-0.8.5
  (crate-source "regex-syntax" "0.8.5"
                "0p41p3hj9ww7blnbwbj9h7rwxzxg0c1hvrdycgys8rxyhqqw859b"))

(define rust-regex-syntax-0.8.6
  (crate-source "regex-syntax" "0.8.6"
                "00chjpglclfskmc919fj5aq308ffbrmcn7kzbkz92k231xdsmx6a"))

(define rust-regex-syntax-0.8.9
  (crate-source "regex-syntax" "0.8.9"
                "0k0a47r1rcl794wj8a948niakbg081s5pp5nlgcbmmr2iy3qfs59"))

(define rust-relm4-0.9.1
  (crate-source "relm4" "0.9.1"
                "1las0fjxmiblq9bv0q0hiqszk3swghwfr0sc80dfmkx8q59pb0rh"))

(define rust-relm4-css-0.9.0
  (crate-source "relm4-css" "0.9.0"
                "1y9prwy1jrwznajayqz4y3dhyqkn9cy322xnhz3ds76zax2r4fqx"))

(define rust-relm4-macros-0.9.1
  (crate-source "relm4-macros" "0.9.1"
                "1pnx5spw2akrjqrgcgayig992bx94jypk9hc21yqa6j4ams5m2as"))

(define rust-reqwest-0.12.23
  (crate-source "reqwest" "0.12.23"
                "1svw1k0jx17cmlwhixwqfv3bgpjapciw7klkghnd9cljh16g6afl"))

(define rust-reqwest-0.12.26
  (crate-source "reqwest" "0.12.26"
                "03r4pvw7kp4c7aa6r4gpyrlbbq3adb6b11kh4xh6mjmgv6r18k1v"))

(define rust-reqwest-0.12.28
  (crate-source "reqwest" "0.12.28"
                "0iqidijghgqbzl3bjg5hb4zmigwa4r612bgi0yiq0c90b6jkrpgd"))

(define rust-rfd-0.15.3
  (crate-source "rfd" "0.15.3"
                "0p87ax7bqk2n8larbrrx0vqvwzkfkfl98igfabiam0nwixs49j40"))

(define rust-rfd-0.15.4
  (crate-source "rfd" "0.15.4"
                "1vgbfi32843dysqdq3fv604qzqll8al82z9dqwsldyngwrhywazg"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rustc-demangle-0.1.25
  (crate-source "rustc-demangle" "0.1.25"
                "0kxq6m0drr40434ch32j31dkg00iaf4zxmqg7sqxajhcz0wng7lq"))

(define rust-rustc-demangle-0.1.26
  (crate-source "rustc-demangle" "0.1.26"
                "1kja3nb0yhlm4j2p1hl8d7sjmn2g9fa1s4pj0qma5kj2lcndkxsn"))

(define rust-rustc-demangle-0.1.27
  (crate-source "rustc-demangle" "0.1.27"
                "17f0jl6lgsy8kwxdzxp3s2wmipvlpna03kkc4vkqr1gwv5lqh2xm"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.1
  (crate-source "rustc-hash" "2.1.1"
                "03gz5lvd9ghcwsal022cgkq67dmimcgdjghfb5yb5d352ga06xrm"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.0.7
  (crate-source "rustix" "1.0.7"
                "0rhjh16bnxi86nrn9qwcnw5632mvd5m1vdy61s4n9zz7mzb867n7"))

(define rust-rustix-1.1.2
  (crate-source "rustix" "1.1.2"
                "0gpz343xfzx16x82s1x336n0kr49j02cvhgxdvaq86jmqnigh5fd"))

(define rust-rustix-1.1.3
  (crate-source "rustix" "1.1.3"
                "0d0z2zcw4rwzni1hm8snw8xdxwwrij336m31c4ghq66cghj9wv0l"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.23.29
  (crate-source "rustls" "0.23.29"
                "1lcvzvzqk8xx8jzg0x5v3mkqgwkwr7v6zdq8zw8rp6xj74h3i494"))

(define rust-rustls-0.23.35
  (crate-source "rustls" "0.23.35"
                "13xxk2qqchibd7pr0laqq6pzayx9xm4rb45d8rz68kvxday58gsk"))

(define rust-rustls-0.23.36
  (crate-source "rustls" "0.23.36"
                "06w0077ssk3blpp93613lkny046mwj0nhxjgc7cmg9nf70yz6rf6"))

(define rust-rustls-native-certs-0.6.3
  (crate-source "rustls-native-certs" "0.6.3"
                "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pki-types-1.12.0
  (crate-source "rustls-pki-types" "1.12.0"
                "0yawbdpix8jif6s8zj1p2hbyb7y3bj66fhx0y7hyf4qh4964m6i2"))

(define rust-rustls-pki-types-1.13.1
  (crate-source "rustls-pki-types" "1.13.1"
                "134hjrwxzrkiag11psaml4gv75f4a9m307cc8rr05fjlbyfhz33h"))

(define rust-rustls-pki-types-1.14.0
  (crate-source "rustls-pki-types" "1.14.0"
                "1p9zsgslvwzzkzhm6bqicffqndr4jpx67992b0vl0pi21a5hy15y"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.103.4
  (crate-source "rustls-webpki" "0.103.4"
                "1z4jmmgasjgk9glb160a66bshvgifa64mgfjrkqp7dy1w158h5qa"))

(define rust-rustls-webpki-0.103.8
  (crate-source "rustls-webpki" "0.103.8"
                "0lpymb84bi5d2pm017n39nbiaa5cd046hgz06ir29ql6a8pzmz9g"))

(define rust-rustls-webpki-0.103.9
  (crate-source "rustls-webpki" "0.103.9"
                "0lwg1nnyv7pp2lfwwjhy81bxm233am99jnsp3iymdhd6k8827pyp"))

(define rust-rustversion-1.0.21
  (crate-source "rustversion" "1.0.21"
                "07bb1xx05hhwpnl43sqrhsmxyk5sd5m5baadp19nxp69s9xij3ca"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schannel-0.1.27
  (crate-source "schannel" "0.1.27"
                "0gbbhy28v72kd5iina0z2vcdl3vz63mk5idvkzn5r52z6jmfna8z"))

(define rust-schannel-0.1.28
  (crate-source "schannel" "0.1.28"
                "1qb6s5gyxfz2inz753a4z3mc1d266mwvz0c5w7ppd3h44swq27c9"))

(define rust-scoped-tls-1.0.1
  (crate-source "scoped-tls" "1.0.1"
                "15524h04mafihcvfpgxd8f4bgc3k95aclz8grjkg9a0rxcvn9kz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-sct-0.7.1
  (crate-source "sct" "0.7.1"
                "056lmi2xkzdg1dbai6ha3n57s18cbip4pnmpdhyljli3m99n216s"))

(define rust-security-framework-2.11.1
  (crate-source "security-framework" "2.11.1"
                "00ldclwx78dm61v7wkach9lcx76awlrv0fdgjdwch4dmy12j4yw9"))

(define rust-security-framework-sys-2.14.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.14.0"
                "0chwn01qrnvs59i5220bymd38iddy4krbnmfnhf4k451aqfj7ns9"))

(define rust-security-framework-sys-2.15.0
  ;; TODO: Check bundled sources.
  (crate-source "security-framework-sys" "2.15.0"
                "1h6mijxnfrwvl1y4dzwn3m877j6dqp9qn3g37i954j5czazhq7yc"))

(define rust-security-framework-sys-2.16.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "security-framework-sys" "2.16.0"
                "06p6x6s8jysrkay1glazxl0r3drwsxwrhjh30lka9acjn1rqc71j"))

(define rust-self-cell-0.10.3
  (crate-source "self_cell" "0.10.3"
                "0pci3zh23b7dg6jmlxbn8k4plb7hcg5jprd1qiz0rp04p1ilskp1"))

(define rust-self-cell-1.2.0
  (crate-source "self_cell" "1.2.0"
                "0jg70srf4hzrw96x8iclgf6i8dfgm1x8ds2i7yzcgq0i8njraz8g"))

(define rust-semver-1.0.26
  (crate-source "semver" "1.0.26"
                "1l5q2vb8fjkby657kdyfpvv40x2i2xqq9bg57pxqakfj92fgmrjn"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-serde-1.0.219
  (crate-source "serde" "1.0.219"
                "1dl6nyxnsi82a197sd752128a4avm6mxnscywas1jq30srp2q3jz"))

(define rust-serde-1.0.226
  (crate-source "serde" "1.0.226"
                "1zcm2asp9fiphbp0k96whabw02kiiqgzxhbyz85vc92v088n9jhd"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.226
  (crate-source "serde_core" "1.0.226"
                "1936x6cpqgyq57nm7qi416dsc4fiq3jv6d7vh74xmfgdk4wscaxs"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.219
  (crate-source "serde_derive" "1.0.219"
                "001azhjmj7ya52pmfiw4ppxm16nd44y15j2pf5gkcwrcgz7pc0jv"))

(define rust-serde-derive-1.0.226
  (crate-source "serde_derive" "1.0.226"
                "0cyvkilp34an3f90b0idw0jjsyq20h7v47gsp8qkfmrl5zi3mdcd"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.140
  (crate-source "serde_json" "1.0.140"
                "0wwkp4vc20r87081ihj3vpyz5qf7wqkqipq17v99nv6wjrp8n1i0"))

(define rust-serde-json-1.0.145
  (crate-source "serde_json" "1.0.145"
                "1767y6kxjf7gwpbv8bkhgwc50nhg46mqwm9gy9n122f7v1k6yaj0"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-repr-0.1.20
  (crate-source "serde_repr" "0.1.20"
                "1755gss3f6lwvv23pk7fhnjdkjw7609rcgjlr8vjg6791blf6php"))

(define rust-serde-spanned-0.6.9
  (crate-source "serde_spanned" "0.6.9"
                "18vmxq6qfrm110caszxrzibjhy2s54n1g5w1bshxq9kjmz7y0hdz"))

(define rust-serde-spanned-1.0.0
  (crate-source "serde_spanned" "1.0.0"
                "10rv91337k8x8zmfir4h8aiwmwgkq07gdv7h0jxhcwwgk10lqws0"))

(define rust-serde-spanned-1.0.2
  (crate-source "serde_spanned" "1.0.2"
                "1vh4kcnzhw0fbr1jhg41p8yybnp5gmpnh171fy25bgn2a8s7h5sl"))

(define rust-serde-urlencoded-0.7.1
  (crate-source "serde_urlencoded" "0.7.1"
                "1zgklbdaysj3230xivihs30qi5vkhigg323a9m62k8jwf4a1qjfk"))

(define rust-sha1-0.10.6
  (crate-source "sha1" "0.10.6"
                "1fnnxlfg08xhkmwf2ahv634as30l1i3xhlhkvxflmasi5nd85gz3"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shlex-1.3.0
  (crate-source "shlex" "1.3.0"
                "0r1y6bv26c1scpxvhg2cabimrmwgbp4p3wy6syj9n0c4s3q2znhg"))

(define rust-signal-hook-registry-1.4.5
  (crate-source "signal-hook-registry" "1.4.5"
                "042lkqrpnlrgvrrcirgigxyp1zk70d8v0fsr5w7a18k3bw2vh0wj"))

(define rust-signal-hook-registry-1.4.6
  (crate-source "signal-hook-registry" "1.4.6"
                "12y2v1ms5z111fymaw1v8k93m5chnkp21h0jknrydkj8zydp395j"))

(define rust-simd-adler32-0.3.7
  (crate-source "simd-adler32" "0.3.7"
                "1zkq40c3iajcnr5936gjp9jjh1lpzhy44p3dq3fiw75iwr1w2vfn"))

(define rust-simd-adler32-0.3.8
  (crate-source "simd-adler32" "0.3.8"
                "18lx2gdgislabbvlgw5q3j5ssrr77v8kmkrxaanp3liimp2sc873"))

(define rust-slab-0.4.10
  (crate-source "slab" "0.4.10"
                "03f5a9gdp33mngya4qwq2555138pj74pl015scv57wsic5rikp04"))

(define rust-slab-0.4.11
  (crate-source "slab" "0.4.11"
                "12bm4s88rblq02jjbi1dw31984w61y2ldn13ifk5gsqgy97f8aks"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-socket2-0.5.10
  (crate-source "socket2" "0.5.10"
                "0y067ki5q946w91xlz2sb175pnfazizva6fi3kfp639mxnmpc8z2"))

(define rust-socket2-0.6.0
  (crate-source "socket2" "0.6.0"
                "01qqdzfnr0bvdwq6wl56c9c4m2cvbxn43dfpcv8gjx208sph8d93"))

(define rust-socket2-0.6.2
  (crate-source "socket2" "0.6.2"
                "1q073zkvz96h216mfz6niqk2kjqrgqv2va6zj34qh84zv4xamx46"))

(define rust-spin-0.9.8
  (crate-source "spin" "0.9.8"
                "0rvam5r0p3a6qhc18scqpvpgb3ckzyqxpgdfyjnghh8ja7byi039"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-syn-2.0.104
  (crate-source "syn" "2.0.104"
                "0h2s8cxh5dsh9h41dxnlzpifqqn59cqgm0kljawws61ljq2zgdhp"))

(define rust-syn-2.0.106
  (crate-source "syn" "2.0.106"
                "19mddxp1ia00hfdzimygqmr1jqdvyl86k48427bkci4d08wc9rzd"))

(define rust-syn-2.0.115
  (crate-source "syn" "2.0.115"
                "04iw4irs1kp4lcs0rl6zmkg8kg2x5lkc8wj9lqzi2a5c439lwqbf"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sysinfo-0.35.2
  (crate-source "sysinfo" "0.35.2"
                "0vj9j8m10j56a41552f1qckv3isnjqs3rsvsgyjj9czj9wzglgrw"))

(define rust-system-deps-7.0.5
  (crate-source "system-deps" "7.0.5"
                "1nzzhqm3nnzacpcs5q1rlbzxrb1hq6xl5g8mqqnnv2ds1jm57gp4"))

(define rust-tar-0.4.44
  (crate-source "tar" "0.4.44"
                "0yk69a8j9xv51mdcy0853jai5zh1pd9yn456q4cpmj0js9w3i1hx"))

(define rust-target-lexicon-0.13.2
  (crate-source "target-lexicon" "0.13.2"
                "16m6smfz533im9dyxfhnzmpi4af75g2iii36ylc4gfmqvf6gf0p5"))

(define rust-tempfile-3.20.0
  (crate-source "tempfile" "3.20.0"
                "18fnp7mjckd9c9ldlb2zhp1hd4467y2hpvx9l50j97rlhlwlx9p8"))

(define rust-tempfile-3.22.0
  (crate-source "tempfile" "3.22.0"
                "0lza9r7dzm4k9fghw24yql6iz59wq8xgs46a7i29ir6xz88lvyl4"))

(define rust-tempfile-3.23.0
  (crate-source "tempfile" "3.23.0"
                "05igl2gml6z6i2va1bv49f9f1wb3f752c2i63lvlb9s2vxxwfc9d"))

(define rust-tempfile-3.25.0
  (crate-source "tempfile" "3.25.0"
                "1wg5jnzbgpb1wmw396v31f0c70dvj5mpik7rk7fzdccmghgpjdh1"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.12
  (crate-source "thiserror" "2.0.12"
                "024791nsc0np63g2pq30cjf9acj38z3jwx9apvvi8qsqmqnqlysn"))

(define rust-thiserror-2.0.16
  (crate-source "thiserror" "2.0.16"
                "1h30bqyjn5s9ypm668yd9849371rzwk185klwgjg503k2hadcrrl"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.12
  (crate-source "thiserror-impl" "2.0.12"
                "07bsn7shydaidvyyrm7jz29vp78vrxr9cr9044rfmn078lmz8z3z"))

(define rust-thiserror-impl-2.0.16
  (crate-source "thiserror-impl" "2.0.16"
                "0q3r1ipr1rhff6cgrcvc0njffw17rpcqz9hdc7p754cbqkhinpkc"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-time-0.3.41
  (crate-source "time" "0.3.41"
                "0h0cpiyya8cjlrh00d2r72bmgg4lsdcncs76qpwy0rn2kghijxla"))

(define rust-time-0.3.44
  (crate-source "time" "0.3.44"
                "179awlwb36zly3nmz5h9awai1h4pbf1d83g2pmvlw4v1pgixkrwi"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-core-0.1.4
  (crate-source "time-core" "0.1.4"
                "0z5h9fknvdvbs2k2s1chpi3ab3jvgkfhdnqwrvixjngm263s7sf9"))

(define rust-time-core-0.1.6
  (crate-source "time-core" "0.1.6"
                "0sqwhg7n47gbffyr0zhipqcnskxgcgzz1ix8wirqs2rg3my8x1j0"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-macros-0.2.22
  (crate-source "time-macros" "0.2.22"
                "0jcaxpw220han2bzbrdlpqhy1s5k9i8ri3lw6n5zv4zcja9p69im"))

(define rust-time-macros-0.2.24
  (crate-source "time-macros" "0.2.24"
                "1wzb6hnl35856f58cx259q7ijc4c7yis0qsnydvw5n8jbw9b1krh"))

(define rust-tinystr-0.8.1
  (crate-source "tinystr" "0.8.1"
                "12sc6h3hnn6x78iycm5v6wrs2xhxph0ydm43yyn7gdfw8l8nsksx"))

(define rust-tinystr-0.8.2
  (crate-source "tinystr" "0.8.2"
                "0sa8z88axdsf088hgw5p4xcyi6g3w3sgbb6qdp81bph9bk2fkls2"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.46.1
  (crate-source "tokio" "1.46.1"
                "05sxldy7kcgysnxyzz1h1l8j3d9mjyqfh7r48ni27gmg9lsa5hqc"))

(define rust-tokio-1.47.1
  (crate-source "tokio" "1.47.1"
                "0f2hp5v3payg6x04ijj67si1wsdhksskhmjs2k9p5f7bmpyrmr49"))

(define rust-tokio-1.49.0
  (crate-source "tokio" "1.49.0"
                "11ix3pl03s0bp71q3wddrbf8xr0cpn47d7fzr6m42r3kswy918kj"))

(define rust-tokio-rustls-0.26.2
  (crate-source "tokio-rustls" "0.26.2"
                "16wf007q3584j46wc4s0zc4szj6280g23hka6x6bgs50l4v7nwlf"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-util-0.7.16
  (crate-source "tokio-util" "0.7.16"
                "1r9wdrg1k5hna3m0kc8kcb8jdb6n52g7vnw93kw2xxw4cyc7qc0l"))

(define rust-tokio-util-0.7.17
  (crate-source "tokio-util" "0.7.17"
                "152m2rp40bjphca5j581csczarvvr974zvwpzpldcwv0wygi9yif"))

(define rust-tokio-util-0.7.18
  (crate-source "tokio-util" "0.7.18"
                "1600rd47pylwn7cap1k7s5nvdaa9j7w8kqigzp1qy7mh0p4cxscs"))

(define rust-toml-0.8.23
  (crate-source "toml" "0.8.23"
                "0qnkrq4lm2sdhp3l6cb6f26i8zbnhqb7mhbmksd550wxdfcyn6yw"))

(define rust-toml-0.9.2
  (crate-source "toml" "0.9.2"
                "1b0fcp0la720p82vcsv3lrkdgsz1lmhv02rfj2bi19rgq6bfw2pd"))

(define rust-toml-0.9.7
  (crate-source "toml" "0.9.7"
                "187av4nsjc0cdfixpc24sqpxqwy5ijvdm7hd9yfsqx94pzcybr80"))

(define rust-toml-datetime-0.6.11
  (crate-source "toml_datetime" "0.6.11"
                "077ix2hb1dcya49hmi1avalwbixmrs75zgzb3b2i7g2gizwdmk92"))

(define rust-toml-datetime-0.7.0
  (crate-source "toml_datetime" "0.7.0"
                "1qwivxqkjxxwcqsvfhxnphpwphci0grdfk197wyxfn1gj0z1rpms"))

(define rust-toml-datetime-0.7.2
  (crate-source "toml_datetime" "0.7.2"
                "1hgff8gdk9yx7dljkqfijmj0sc5ln4xhpj045divdhi7xifhiw9j"))

(define rust-toml-edit-0.22.27
  (crate-source "toml_edit" "0.22.27"
                "16l15xm40404asih8vyjvnka9g0xs9i4hfb6ry3ph9g419k8rzj1"))

(define rust-toml-edit-0.23.6
  (crate-source "toml_edit" "0.23.6"
                "0jqq4wz6is0497a42m0wh4j3x4vgp70wrlndd57zzzc61rygxvzk"))

(define rust-toml-parser-1.0.3
  (crate-source "toml_parser" "1.0.3"
                "09x6i0b57lwc7yn6w1kbd2ypm4vpcrgd2vdax7h745g77g1r7y2c"))

(define rust-toml-writer-1.0.0
  (crate-source "toml_writer" "1.0.0"
                "1m8gd4890k85j96rbqgq83dbfh3gsvkcb3rypp579pj851zj2ydn"))

(define rust-toml-writer-1.0.3
  (crate-source "toml_writer" "1.0.3"
                "0281l7bgchmlbvxmci01p9x2w5br9p61ylns5ji65rbc24yacqyi"))

(define rust-tower-0.5.2
  (crate-source "tower" "0.5.2"
                "1ybmd59nm4abl9bsvy6rx31m4zvzp5rja2slzpn712y9b68ssffh"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.6.6
  (crate-source "tower-http" "0.6.6"
                "1wh51y4rf03f91c6rvli6nwzsarx7097yx6sqlm75ag27pbjzj5d"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.41
  (crate-source "tracing" "0.1.41"
                "1l5xrzyjfyayrwhvhldfnwdyligi1mpqm8mzbi2m1d6y6p2hlkkq"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.30
  (crate-source "tracing-attributes" "0.1.30"
                "00v9bhfgfg3v101nmmy7s3vdwadb7ngc8c1iw6wai9vj9sv3lf41"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.34
  (crate-source "tracing-core" "0.1.34"
                "0y3nc4mpnr79rzkrcylv5f5bnjjp19lsxwis9l4kzs97ya0jbldr"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.19
  (crate-source "tracing-subscriber" "0.3.19"
                "0220rignck8072i89jjsh140vmh14ydwpdwnifyaf3xcnpn9s678"))

(define rust-tracing-subscriber-0.3.20
  (crate-source "tracing-subscriber" "0.3.20"
                "1m9447bxq7236avgl6n5yb2aqwplrghm61dgipw03mh7ad7s2m10"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-type-map-0.5.1
  (crate-source "type-map" "0.5.1"
                "143v32wwgpymxfy4y8s694vyq0wdi7li4s5dmms5w59nj2yxnc6b"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-typenum-1.19.0
  (crate-source "typenum" "1.19.0"
                "1fw2mpbn2vmqan56j1b3fbpcdg80mz26fm53fs16bq5xcq84hban"))

(define rust-uds-windows-1.1.0
  (crate-source "uds_windows" "1.1.0"
                "1fb4y65pw0rsp0gyfyinjazlzxz1f6zv7j4zmb20l5pxwv1ypnl9"))

(define rust-unic-langid-0.9.6
  (crate-source "unic-langid" "0.9.6"
                "01bx59sqsx2jz4z7ppxq9kldcjq9dzadkmb2dr7iyc85kcnab2x2"))

(define rust-unic-langid-impl-0.9.6
  (crate-source "unic-langid-impl" "0.9.6"
                "0n66kdan4cz99n8ra18i27f7w136hmppi4wc0aa7ljsd0h4bzqfw"))

(define rust-unic-langid-macros-0.9.6
  (crate-source "unic-langid-macros" "0.9.6"
                "09gwlpdzxnzhywvarfm43d7g1672lwak6ahq2kfplv9l5sw7x5fm"))

(define rust-unic-langid-macros-impl-0.9.6
  (crate-source "unic-langid-macros-impl" "0.9.6"
                "1dbmgybjxn4b3a7mb21grc5r98xwal9h1cgc46w39bg3imi9l951"))

(define rust-unicode-ident-1.0.18
  (crate-source "unicode-ident" "1.0.18"
                "04k5r6sijkafzljykdq26mhjpmhdx4jwzvn1lh90g9ax9903jpss"))

(define rust-unicode-ident-1.0.19
  (crate-source "unicode-ident" "1.0.19"
                "17bx1j1zf6b9j3kpyf74mraary7ava3984km0n8kh499h5a58fpn"))

(define rust-unicode-ident-1.0.23
  (crate-source "unicode-ident" "1.0.23"
                "17m0v64a09qyp1b4jy2d9ywinp1ic2rd2jyxcjdvwy4qm0wd0zak"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

(define rust-url-2.5.4
  (crate-source "url" "2.5.4"
                "0q6sgznyy2n4l5lm16zahkisvc9nip9aa5q1pps7656xra3bdy1j"))

(define rust-url-2.5.7
  (crate-source "url" "2.5.7"
                "0nzghdv0kcksyvri0npxbjzyx2ihprks5k590y77bld355m17g08"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.17.0
  (crate-source "uuid" "1.17.0"
                "07ckq4fdiygy02gmislzfp727hx9zw6lskk9dbcds5ax3sfikx1w"))

(define rust-uuid-1.18.1
  (crate-source "uuid" "1.18.1"
                "18kh01qmfayn4psap52x8xdjkzw2q8bcbpnhhxjs05dr22mbi1rg"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.0
  (crate-source "version-compare" "0.2.0"
                "12y9262fhjm1wp0aj3mwhads7kv0jz8h168nn5fb8b43nwf9abl5"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasi-0.14.2+wasi-0.2.4
  (crate-source "wasi" "0.14.2+wasi-0.2.4"
                "1cwcqjr3dgdq8j325awgk8a715h0hg0f7jqzsb077n4qm6jzk0wn"))

(define rust-wasi-0.14.7+wasi-0.2.4
  (crate-source "wasi" "0.14.7+wasi-0.2.4"
                "133fq3mq7h65mzrsphcm7bbbx1gsz7srrbwh01624zin43g7hd48"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip2-1.0.2+wasi-0.2.9
  (crate-source "wasip2" "1.0.2+wasi-0.2.9"
                "1xdw7v08jpfjdg94sp4lbdgzwa587m5ifpz6fpdnkh02kwizj5wm"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.100
  (crate-source "wasm-bindgen" "0.2.100"
                "1x8ymcm6yi3i1rwj78myl1agqv2m86i648myy3lc97s9swlqkp0y"))

(define rust-wasm-bindgen-0.2.103
  (crate-source "wasm-bindgen" "0.2.103"
                "069qhf7yrl4jymzjzvwsmcmw96al639xim4scigpy5qapngsc45b"))

(define rust-wasm-bindgen-0.2.104
  (crate-source "wasm-bindgen" "0.2.104"
                "0b8f4l6pqm0bz0lj5xgwmchb6977n71vmh7srd0axwg93b011nn1"))

(define rust-wasm-bindgen-0.2.108
  (crate-source "wasm-bindgen" "0.2.108"
                "0rl5pn80sdhj2p2r28lp3k50a8mpppzgwzssz2f3jdqyxhq4l0k4"))

(define rust-wasm-bindgen-backend-0.2.100
  (crate-source "wasm-bindgen-backend" "0.2.100"
                "1ihbf1hq3y81c4md9lyh6lcwbx6a5j0fw4fygd423g62lm8hc2ig"))

(define rust-wasm-bindgen-backend-0.2.103
  (crate-source "wasm-bindgen-backend" "0.2.103"
                "070x7fjnnvzm2y3a5j29wmss4z547cjdx3rnpixh19j56m105dqb"))

(define rust-wasm-bindgen-backend-0.2.104
  (crate-source "wasm-bindgen-backend" "0.2.104"
                "069vnhhn2j4w2gwd8rch6g8d3iwkrgi45fas6i3qm7glcrd9l737"))

(define rust-wasm-bindgen-futures-0.4.50
  (crate-source "wasm-bindgen-futures" "0.4.50"
                "0q8ymi6i9r3vxly551dhxcyai7nc491mspj0j1wbafxwq074fpam"))

(define rust-wasm-bindgen-futures-0.4.53
  (crate-source "wasm-bindgen-futures" "0.4.53"
                "0rrglk9fjwk69adwca1skn10v9qlhacmbfxd16aq6mhj8bzj3cm0"))

(define rust-wasm-bindgen-futures-0.4.54
  (crate-source "wasm-bindgen-futures" "0.4.54"
                "0p5c10vfd7p7c607x3cgyfw9h77z1k33d6zzw2x77k3qwi0qs0vy"))

(define rust-wasm-bindgen-futures-0.4.58
  (crate-source "wasm-bindgen-futures" "0.4.58"
                "0vqywn9df5i6mms3sw47v3kj7rzx8ryghqq0xb4jk05fs1zyg9kh"))

(define rust-wasm-bindgen-macro-0.2.100
  (crate-source "wasm-bindgen-macro" "0.2.100"
                "01xls2dvzh38yj17jgrbiib1d3nyad7k2yw9s0mpklwys333zrkz"))

(define rust-wasm-bindgen-macro-0.2.103
  (crate-source "wasm-bindgen-macro" "0.2.103"
                "18481jkmczv4j0m747ypb0k1acq093hhbdhpb4sr856r27sg8rgw"))

(define rust-wasm-bindgen-macro-0.2.104
  (crate-source "wasm-bindgen-macro" "0.2.104"
                "06d1m5bg272h6jabq0snm7c50fifjz6r20f5hqlmz7y5wivh99kw"))

(define rust-wasm-bindgen-macro-0.2.108
  (crate-source "wasm-bindgen-macro" "0.2.108"
                "026nnvakp0w6j3ghpcxn31shj9wx8bv8x7nk3gkk40klkjfj72q0"))

(define rust-wasm-bindgen-macro-support-0.2.100
  (crate-source "wasm-bindgen-macro-support" "0.2.100"
                "1plm8dh20jg2id0320pbmrlsv6cazfv6b6907z19ys4z1jj7xs4a"))

(define rust-wasm-bindgen-macro-support-0.2.103
  (crate-source "wasm-bindgen-macro-support" "0.2.103"
                "0clsx611pday95s6wg8pndvrd8xknsaf20d40kk8x2irj6lh7h7z"))

(define rust-wasm-bindgen-macro-support-0.2.104
  (crate-source "wasm-bindgen-macro-support" "0.2.104"
                "1mr18kx7ima1pmsqlkk982q4a0vf3r8s1x6901jb59sd1prd41wz"))

(define rust-wasm-bindgen-macro-support-0.2.108
  (crate-source "wasm-bindgen-macro-support" "0.2.108"
                "0m9sj475ypgifbkvksjsqs2gy3bq96f87ychch784m4gspiblmjj"))

(define rust-wasm-bindgen-shared-0.2.100
  (crate-source "wasm-bindgen-shared" "0.2.100"
                "0gffxvqgbh9r9xl36gprkfnh3w9gl8wgia6xrin7v11sjcxxf18s"))

(define rust-wasm-bindgen-shared-0.2.103
  (crate-source "wasm-bindgen-shared" "0.2.103"
                "1kx13fvmlxxaxf04vm3b14437hyq92zdy89pvcaclc54xzs3fg19"))

(define rust-wasm-bindgen-shared-0.2.104
  (crate-source "wasm-bindgen-shared" "0.2.104"
                "1la1xj9v3gmawnlyi7lc3mb3xi447r6frb98hi2fb9m1nb47vmms"))

(define rust-wasm-bindgen-shared-0.2.108
  (crate-source "wasm-bindgen-shared" "0.2.108"
                "04ix7v99rvj5730553j58pqsrwpf9sqazr60y3cchx5cr60ba08z"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-wayland-backend-0.3.11
  (crate-source "wayland-backend" "0.3.11"
                "0dcvwkhz45gsm7f9dwr31pxijkhpza09a4vb3blsv9a8631k6fk7"))

(define rust-wayland-client-0.31.11
  (crate-source "wayland-client" "0.31.11"
                "17a4vl5qw4jnnh2azm0d3kcpajyb9qz4psv448zpj86w83l4fsn6"))

(define rust-wayland-protocols-0.32.9
  (crate-source "wayland-protocols" "0.32.9"
                "00cripl4m7hzhl0gzp4bqayal8n0zlf1llnj7cl73zgvfpnr19zg"))

(define rust-wayland-scanner-0.31.7
  (crate-source "wayland-scanner" "0.31.7"
                "1qqalp551blcxjzx80zvs7ckc19k966892zxpm81kacxqjfixjsl"))

(define rust-wayland-sys-0.31.7
  ;; TODO: Check bundled sources.
  (crate-source "wayland-sys" "0.31.7"
                "0hk157yawv9y7aj7fxbldhlvv8p33c65v3nv85mq4m91h919p51l"))

(define rust-web-sys-0.3.77
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.77"
                "1lnmc1ffbq34qw91nndklqqm75rasaffj2g4f8h1yvqqz4pdvdik"))

(define rust-web-sys-0.3.80
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.80"
                "1p4nwlrjvaymzas40x1i0n6mzn8pilzl6jwcswlw97c6bs4k9rzv"))

(define rust-web-sys-0.3.81
  ;; TODO: Check bundled sources.
  (crate-source "web-sys" "0.3.81"
                "0871ifd79ni9813sp5amk7wb3avznkijlsly2ap4r9r4m4bw8rwk"))

(define rust-web-sys-0.3.85
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.85"
                "1645c202gyw21m6kxw4ya81vrapl40hlb8m9iqhjj8fra7jk4bii"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-roots-0.25.4
  (crate-source "webpki-roots" "0.25.4"
                "1qgqa615gc1cgklls4bkjp9jv9pvv3jnl82lc6wd7dkximywa82z"))

(define rust-webpki-roots-1.0.1
  (crate-source "webpki-roots" "1.0.1"
                "00mm4bhkvis59pm2a7yz3ak6q8rykcj0ddj09wxfskm285ddv0l7"))

(define rust-webpki-roots-1.0.4
  (crate-source "webpki-roots" "1.0.4"
                "07jp2zgj3hjb60m1nwrasixdwazmzhh9y4bryy66wz6457q8x1xj"))

(define rust-webpki-roots-1.0.6
  (crate-source "webpki-roots" "1.0.6"
                "1v8brkarm4spqkjs6y5b67xixnz4zlg33d1wwxigz4rr0qyazkr2"))

(define rust-whatadistro-0.1.0
  (crate-source "whatadistro" "0.1.0"
                "04yr0lnmpjmr8vhdjy5v47zinqs5fic6289z108rb02r9ynyp5qw"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-util-0.1.9
  (crate-source "winapi-util" "0.1.9"
                "1fqhkcl9scd230cnfj8apfficpf5c9vhwnk4yy9xfc1sw69iq8ng"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-wincompatlib-0.7.7
  (crate-source "wincompatlib" "0.7.7"
                "016dypkxhk9qi218n8halhp5w3aik5q7w489sy9zq2vmsc01dr5a"))

(define rust-windows-0.61.3
  (crate-source "windows" "0.61.3"
                "14v8dln7i4ccskd8danzri22bkjkbmgzh284j3vaxhd4cykx7awv"))

(define rust-windows-aarch64-gnullvm-0.48.5
  (crate-source "windows_aarch64_gnullvm" "0.48.5"
                "1n05v7qblg1ci3i567inc7xrkmywczxrs1z3lj3rkkxw18py6f1b"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.48.5
  (crate-source "windows_aarch64_msvc" "0.48.5"
                "1g5l4ry968p73g6bg6jgyvy9lb8fyhcs54067yzxpcpkf44k2dfw"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-future-0.2.1
  (crate-source "windows-future" "0.2.1"
                "13mdzcdn51ckpzp3frb8glnmkyjr1c30ym9wnzj9zc97hkll2spw"))

(define rust-windows-i686-gnu-0.48.5
  (crate-source "windows_i686_gnu" "0.48.5"
                "0gklnglwd9ilqx7ac3cn8hbhkraqisd0n83jxzf9837nvvkiand7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.48.5
  (crate-source "windows_i686_msvc" "0.48.5"
                "01m4rik437dl9rdf0ndnm2syh10hizvq0dajdkv2fjqcywrw4mcg"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.60.0
  (crate-source "windows-implement" "0.60.0"
                "0dm88k3hlaax85xkls4gf597ar4z8m5vzjjagzk910ph7b8xszx4"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.59.1
  (crate-source "windows-interface" "0.59.1"
                "1a4zr8740gyzzhq02xgl6vx8l669jwfby57xgf0zmkcdkyv134mx"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.0
  (crate-source "windows-link" "0.2.0"
                "0r9w2z96d5phmm185aq92z54jp9h2nqisa4wgc71idxbc436rr25"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-sys-0.48.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.48.0"
                "1aan23v5gs7gya1lc46hqn9mdh8yph3fhxmhxlw36pn6pqc28zb7"))

(define rust-windows-sys-0.52.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.0
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.0"
                "1ajpwsmzfcsa1r7i0dxzvfn24dp3525rcd7aq95ydvdj8171h0g2"))

(define rust-windows-sys-0.61.2
  ;; TODO: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.48.5
  (crate-source "windows-targets" "0.48.5"
                "034ljxqshifs1lan89xwpcy1hp0lhdh4b5n0d2z4fwjx2piacbws"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.2
  (crate-source "windows-targets" "0.53.2"
                "1vwanhx2br7dh8mmrszdbcf01bccjr01mcyxcscxl4ffr7y6jvy6"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-threading-0.1.0
  (crate-source "windows-threading" "0.1.0"
                "19jpn37zpjj2q7pn07dpq0ay300w65qx7wdp13wbp8qf5snn6r5n"))

(define rust-windows-x86-64-gnu-0.48.5
  (crate-source "windows_x86_64_gnu" "0.48.5"
                "13kiqqcvz2vnyxzydjh73hwgigsdr2z1xpzx313kxll34nyhmm2k"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.48.5
  (crate-source "windows_x86_64_gnullvm" "0.48.5"
                "1k24810wfbgz8k48c2yknqjmiigmql6kk3knmddkv8k8g1v54yqb"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.48.5
  (crate-source "windows_x86_64_msvc" "0.48.5"
                "0f4mdp895kkjh9zv8dxvn4pc10xr7839lf5pa9l0193i2pkgr57d"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winnow-0.7.12
  (crate-source "winnow" "0.7.12"
                "159y8inpy86xswmr4yig9hxss0v2fssyqy1kk12504n8jbsfpvgk"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rt-0.39.0
  (crate-source "wit-bindgen-rt" "0.39.0"
                "1hd65pa5hp0nl664m94bg554h4zlhrzmkjsf6lsgsb7yc4734hkg"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-writeable-0.6.1
  (crate-source "writeable" "0.6.1"
                "1fx29zncvbrqzgz7li88vzdm8zvgwgwy2r9bnjqxya09pfwi0bza"))

(define rust-writeable-0.6.2
  (crate-source "writeable" "0.6.2"
                "1fg08y97n6vk7l0rnjggw3xyrii6dcqg54wqaxldrlk98zdy1pcy"))

(define rust-xattr-1.5.1
  (crate-source "xattr" "1.5.1"
                "0299mqwjfayn4g0aq156mc2myirw5bi2b8gqi6x85p2iff1ijfmg"))

(define rust-xattr-1.6.1
  (crate-source "xattr" "1.6.1"
                "0ml1mb43gqasawillql6b344m0zgq8mz0isi11wj8vbg43a5mr1j"))

(define rust-xz-0.1.0
  (crate-source "xz" "0.1.0"
                "0d6sq57g1969hjl5k7gzzdbyr60za9hk8qs9iqz26biazy87d21w"))

(define rust-xz2-0.1.7
  (crate-source "xz2" "0.1.7"
                "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))

(define rust-yoke-0.8.0
  (crate-source "yoke" "0.8.0"
                "1k4mfr48vgi7wh066y11b7v1ilakghlnlhw9snzz8vi2p00vnhaz"))

(define rust-yoke-0.8.1
  (crate-source "yoke" "0.8.1"
                "0m29dm0bf5iakxgma0bj6dbmc3b8qi9b1vaw9sa76kdqmz3fbmkj"))

(define rust-yoke-derive-0.8.0
  (crate-source "yoke-derive" "0.8.0"
                "1dha5jrjz9jaq8kmxq1aag86b98zbnm9lyjrihy5sv716sbkrniq"))

(define rust-yoke-derive-0.8.1
  (crate-source "yoke-derive" "0.8.1"
                "0pbyja133jnng4mrhimzdq4a0y26421g734ybgz8wsgbfhl0andn"))

(define rust-zbus-5.11.0
  (crate-source "zbus" "5.11.0"
                "1xxdxb95h4cyn8w03yrgrxrpy2pr9x7blqyfn9sy7f2z0dny81rd"))

(define rust-zbus-5.8.0
  (crate-source "zbus" "5.8.0"
                "0paigs5x5rf45kk5nfz2d61cs4snhnbjf0bnhacg1rn7igllazsr"))

(define rust-zbus-macros-5.11.0
  (crate-source "zbus_macros" "5.11.0"
                "1jpsvssaxh6fxpbb64dmfnrr8r05rsyfhm32bg63rva7r2lrgrsp"))

(define rust-zbus-macros-5.8.0
  (crate-source "zbus_macros" "5.8.0"
                "1hk271lp6kx8gnc8q7i7mna863pk409cv2dik1xdknfd9phy9j75"))

(define rust-zbus-names-4.2.0
  (crate-source "zbus_names" "4.2.0"
                "15ybdd6zic7simi9wjg0ywrxfq4sxg3z0wiyysadps3cpxj8xrkv"))

(define rust-zerocopy-0.8.26
  (crate-source "zerocopy" "0.8.26"
                "0bvsj0qzq26zc6nlrm3z10ihvjspyngs7n0jw1fz031i7h6xsf8h"))

(define rust-zerocopy-0.8.27
  (crate-source "zerocopy" "0.8.27"
                "0b1870gf2zzlckca69v2k4mqwmf8yh2li37qldnzvvd3by58g508"))

(define rust-zerocopy-0.8.39
  (crate-source "zerocopy" "0.8.39"
                "0jmf1iqns5sq07k3dscsgyc706pycar67rrq4j9nrnzacgb3avfv"))

(define rust-zerocopy-derive-0.8.26
  (crate-source "zerocopy-derive" "0.8.26"
                "10aiywi5qkha0mpsnb1zjwi44wl2rhdncaf3ykbp4i9nqm65pkwy"))

(define rust-zerocopy-derive-0.8.27
  (crate-source "zerocopy-derive" "0.8.27"
                "0c9qrylm2p55dvaplxsl24ma48add9qk4y0d6kjbkllaqvcvill8"))

(define rust-zerocopy-derive-0.8.39
  (crate-source "zerocopy-derive" "0.8.39"
                "05z5yfq0mx3xdqadrgq5sd4d03nl82d9r0vp1qchaip9d4qws8j1"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.8.1
  (crate-source "zeroize" "1.8.1"
                "1pjdrmjwmszpxfd7r860jx54cyk94qk59x13sc307cvr5256glyf"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zeroize-derive-1.4.2
  (crate-source "zeroize_derive" "1.4.2"
                "0sczjlqjdmrp3wn62g7mw6p438c9j4jgp2f9zamd56991mdycdnf"))

(define rust-zeroize-derive-1.4.3
  (crate-source "zeroize_derive" "1.4.3"
                "0bl5vd1lz27p4z336nximg5wrlw5j7jc8fxh7iv6r1wrhhav99c5"))

(define rust-zerotrie-0.2.2
  (crate-source "zerotrie" "0.2.2"
                "15gmka7vw5k0d24s0vxgymr2j6zn2iwl12wpmpnpjgsqg3abpw1n"))

(define rust-zerotrie-0.2.3
  (crate-source "zerotrie" "0.2.3"
                "0lbqznlqazmrwwzslw0ci7p3pqxykrbfhq29npj0gmb2amxc2n9a"))

(define rust-zerovec-0.11.2
  (crate-source "zerovec" "0.11.2"
                "0a2457fmz39k9vrrj3rm82q5ykdhgxgbwfz2r6fa6nq11q4fn1aa"))

(define rust-zerovec-0.11.4
  (crate-source "zerovec" "0.11.4"
                "0fz7j1ns8d86m2fqg2a4bzi5gnh5892bxv4kcr9apwc6a3ajpap7"))

(define rust-zerovec-0.11.5
  (crate-source "zerovec" "0.11.5"
                "00m0p47k2g9mkv505hky5xh3r6ps7v8qc0dy4pspg542jj972a3c"))

(define rust-zerovec-derive-0.11.1
  (crate-source "zerovec-derive" "0.11.1"
                "13zms8hj7vzpfswypwggyfr4ckmyc7v3di49pmj8r1qcz9z275jv"))

(define rust-zerovec-derive-0.11.2
  (crate-source "zerovec-derive" "0.11.2"
                "1wsig4h5j7a1scd5hrlnragnazjny9qjc44hancb6p6a76ay7p7a"))

(define rust-zip-3.0.0
  (crate-source "zip" "3.0.0"
                "024kvq5znpyaqggfkz2807h43m23dww1r53zc1gi1l1fa098hn8j"))

(define rust-zlib-rs-0.5.1
  (crate-source "zlib-rs" "0.5.1"
                "12nvshiq19nd4ksn3453ym9p0kcqf0hpaq301p2iyx9ljzxdjsv2"))

(define rust-zlib-rs-0.5.2
  (crate-source "zlib-rs" "0.5.2"
                "1wh0brb3cci6ifdwwz6xasznlrgb8pr99l1z8i15qpigyj9aw1ig"))

(define rust-zlib-rs-0.6.0
  (crate-source "zlib-rs" "0.6.0"
                "0g3ydblvzjn6lfs04q3myxxmvzn1a660whp9nr137g6chbv8m557"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-zopfli-0.8.2
  (crate-source "zopfli" "0.8.2"
                "1mqc2np2l47l9r1qkdm41ykgsp6m096z3ipchi4ws17m0pj5xz7d"))

(define rust-zopfli-0.8.3
  (crate-source "zopfli" "0.8.3"
                "0jaj5dyh3mks0805h4ldrsh5pwq4i2jc9dc9zwjm91k3gmwxhp7h"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.15+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.15+zstd.1.5.7"
                "0dx2l7dyw1p7x7g3p1pfd25ip36hr22hvmgixm6cgl4pvlyii0gb"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  ;; TODO: Check bundled sources.
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define rust-zvariant-5.6.0
  (crate-source "zvariant" "5.6.0"
                "0vvfrarv330xgvqiwr3ycynfv91qa4mif527v2mid4ikpf03c6yr"))

(define rust-zvariant-5.7.0
  (crate-source "zvariant" "5.7.0"
                "1nwqji0y214dnchq372rsfqzl86d9wgai909s761yay5ffzd77cr"))

(define rust-zvariant-derive-5.6.0
  (crate-source "zvariant_derive" "5.6.0"
                "0262vcpb64xhbxw8gz20k6av88yxmj9dgrgvzzdshng43d86i31s"))

(define rust-zvariant-derive-5.7.0
  (crate-source "zvariant_derive" "5.7.0"
                "13mnhlw8imn6garkqrq4gyxyj7rjnp0hfgqdv5mj4vd44q5zshv6"))

(define rust-zvariant-utils-3.2.0
  (crate-source "zvariant_utils" "3.2.0"
                "0d7wllndiv7vwgapmji3q9sxmzbaqfdxjwkqnx9vbmz58gpdyvp1"))

(define rust-zvariant-utils-3.2.1
  (crate-source "zvariant_utils" "3.2.1"
                "16g5id3h9q85c5vcwdfwkwmwzyladbr2q8x2xinr3xl95wa9v566"))

(define ssss-separator 'end-of-crates)

;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (an-anime-game-launcher =>
                                             (list rust-addr2line-0.24.2
                                              rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.3
                                              rust-allocator-api2-0.2.21
                                              rust-anime-game-core-1.36.3.044a1e8
                                              rust-anime-launcher-sdk-1.32.0.87c4206
                                              rust-anstream-0.6.19
                                              rust-anstyle-1.0.11
                                              rust-anstyle-parse-0.2.7
                                              rust-anstyle-query-1.1.3
                                              rust-anstyle-wincon-3.0.9
                                              rust-anyhow-1.0.98
                                              rust-arbitrary-1.4.1
                                              rust-arrayref-0.3.9
                                              rust-arrayvec-0.7.6
                                              rust-ashpd-0.11.0
                                              rust-async-broadcast-0.7.2
                                              rust-async-recursion-1.1.1
                                              rust-async-trait-0.1.88
                                              rust-atomic-waker-1.1.2
                                              rust-autocfg-1.5.0
                                              rust-backtrace-0.3.75
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.9.1
                                              rust-blake3-1.8.2
                                              rust-block-buffer-0.10.4
                                              rust-block2-0.6.1
                                              rust-bstr-1.12.0
                                              rust-bumpalo-3.19.0
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.10.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cairo-rs-0.20.12
                                              rust-cairo-sys-rs-0.20.10
                                              rust-cc-1.2.29
                                              rust-cfg-expr-0.20.1
                                              rust-cfg-if-1.0.1
                                              rust-cfg-aliases-0.2.1
                                              rust-cipher-0.4.4
                                              rust-colorchoice-1.0.4
                                              rust-concurrent-queue-2.5.0
                                              rust-constant-time-eq-0.3.1
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-crc-3.3.0
                                              rust-crc-catalog-2.4.0
                                              rust-crc32fast-1.4.2
                                              rust-crossbeam-deque-0.8.6
                                              rust-crossbeam-epoch-0.9.18
                                              rust-crossbeam-utils-0.8.21
                                              rust-crypto-common-0.1.6
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.9
                                              rust-deranged-0.4.0
                                              rust-derive-arbitrary-1.4.1
                                              rust-digest-0.10.7
                                              rust-dispatch2-0.2.0
                                              rust-dispatch2-0.3.0
                                              rust-displaydoc-0.2.5
                                              rust-dns-lookup-2.0.4
                                              rust-either-1.15.0
                                              rust-endi-1.1.0
                                              rust-enum-ordinalize-4.3.0
                                              rust-enum-ordinalize-derive-4.3.1
                                              rust-enumflags2-0.7.12
                                              rust-enumflags2-derive-0.7.12
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.13
                                              rust-event-listener-5.4.0
                                              rust-event-listener-strategy-0.5.4
                                              rust-fastrand-2.3.0
                                              rust-field-offset-0.3.6
                                              rust-filetime-0.2.25
                                              rust-flate2-1.1.2
                                              rust-fluent-bundle-0.15.3
                                              rust-fluent-langneg-0.13.0
                                              rust-fluent-syntax-0.11.1
                                              rust-fluent-template-macros-0.13.0
                                              rust-fluent-templates-0.13.0
                                              rust-flume-0.11.1
                                              rust-fnv-1.0.7
                                              rust-form-urlencoded-1.2.1
                                              rust-fragile-2.0.1
                                              rust-fs-extra-1.3.0
                                              rust-futures-0.3.31
                                              rust-futures-channel-0.3.31
                                              rust-futures-core-0.3.31
                                              rust-futures-executor-0.3.31
                                              rust-futures-io-0.3.31
                                              rust-futures-lite-2.6.0
                                              rust-futures-macro-0.3.31
                                              rust-futures-sink-0.3.31
                                              rust-futures-task-0.3.31
                                              rust-futures-util-0.3.31
                                              rust-gdk-pixbuf-0.20.10
                                              rust-gdk-pixbuf-sys-0.20.10
                                              rust-gdk4-0.9.6
                                              rust-gdk4-sys-0.9.6
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.16
                                              rust-getrandom-0.3.3
                                              rust-gimli-0.31.1
                                              rust-gio-0.20.12
                                              rust-gio-sys-0.20.10
                                              rust-glib-0.20.12
                                              rust-glib-build-tools-0.20.0
                                              rust-glib-macros-0.20.12
                                              rust-glib-sys-0.20.10
                                              rust-globset-0.4.16
                                              rust-gobject-sys-0.20.10
                                              rust-graphene-rs-0.20.10
                                              rust-graphene-sys-0.20.10
                                              rust-gsk4-0.9.6
                                              rust-gsk4-sys-0.9.6
                                              rust-gtk4-0.9.7
                                              rust-gtk4-macros-0.9.5
                                              rust-gtk4-sys-0.9.6
                                              rust-h2-0.4.12
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.4
                                              rust-heck-0.5.0
                                              rust-hex-0.4.3
                                              rust-hmac-0.12.1
                                              rust-home-0.5.11
                                              rust-http-1.3.1
                                              rust-http-body-1.0.1
                                              rust-http-body-util-0.1.3
                                              rust-httparse-1.10.1
                                              rust-human-panic-2.0.3
                                              rust-hyper-1.7.0
                                              rust-hyper-rustls-0.27.7
                                              rust-hyper-util-0.1.16
                                              rust-icu-collections-2.0.0
                                              rust-icu-locale-core-2.0.0
                                              rust-icu-normalizer-2.0.0
                                              rust-icu-normalizer-data-2.0.0
                                              rust-icu-properties-2.0.1
                                              rust-icu-properties-data-2.0.1
                                              rust-icu-provider-2.0.0
                                              rust-ident-case-1.0.1
                                              rust-idna-1.0.3
                                              rust-idna-adapter-1.2.1
                                              rust-ignore-0.4.23
                                              rust-indexmap-2.10.0
                                              rust-inout-0.1.4
                                              rust-intl-memoizer-0.5.3
                                              rust-intl-pluralrules-7.0.2
                                              rust-io-uring-0.7.8
                                              rust-ipnet-2.11.0
                                              rust-iri-string-0.7.8
                                              rust-is-docker-0.2.0
                                              rust-is-wsl-0.4.0
                                              rust-is-terminal-polyfill-1.70.1
                                              rust-itoa-1.0.15
                                              rust-jobserver-0.1.33
                                              rust-js-sys-0.3.77
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-libadwaita-0.7.2
                                              rust-libadwaita-sys-0.7.2
                                              rust-libc-0.2.174
                                              rust-libredox-0.1.4
                                              rust-libz-rs-sys-0.5.1
                                              rust-linux-raw-sys-0.4.15
                                              rust-linux-raw-sys-0.9.4
                                              rust-litemap-0.8.0
                                              rust-lock-api-0.4.13
                                              rust-log-0.4.27
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.7.5
                                              rust-memoffset-0.9.1
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.0
                                              rust-mio-1.0.4
                                              rust-nanorand-0.7.0
                                              rust-nix-0.30.1
                                              rust-ntapi-0.4.1
                                              rust-nu-ansi-term-0.46.0
                                              rust-num-conv-0.1.0
                                              rust-objc2-0.6.1
                                              rust-objc2-app-kit-0.3.1
                                              rust-objc2-core-foundation-0.3.1
                                              rust-objc2-encode-4.1.0
                                              rust-objc2-foundation-0.3.1
                                              rust-objc2-io-kit-0.3.1
                                              rust-object-0.36.7
                                              rust-once-cell-1.21.3
                                              rust-once-cell-polyfill-1.70.1
                                              rust-open-5.3.2
                                              rust-openssl-probe-0.1.6
                                              rust-ordered-stream-0.2.0
                                              rust-os-info-3.12.0
                                              rust-overload-0.1.1
                                              rust-pango-0.20.12
                                              rust-pango-sys-0.20.10
                                              rust-parking-2.2.1
                                              rust-pathdiff-0.2.3
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.1
                                              rust-pin-project-lite-0.2.16
                                              rust-pin-utils-0.1.0
                                              rust-pkg-config-0.3.32
                                              rust-plist-1.7.4
                                              rust-pollster-0.4.0
                                              rust-potential-utf-0.1.2
                                              rust-powerfmt-0.2.0
                                              rust-ppv-lite86-0.2.21
                                              rust-proc-macro-crate-3.3.0
                                              rust-proc-macro-hack-0.5.20+deprecated
                                              rust-proc-macro2-1.0.95
                                              rust-protobuf-3.7.2
                                              rust-protobuf-codegen-3.7.2
                                              rust-protobuf-parse-3.7.2
                                              rust-protobuf-support-3.7.2
                                              rust-quick-xml-0.38.0
                                              rust-quinn-0.11.8
                                              rust-quinn-proto-0.11.12
                                              rust-quinn-udp-0.5.13
                                              rust-quote-1.0.40
                                              rust-r-efi-5.3.0
                                              rust-rand-0.9.1
                                              rust-rand-chacha-0.9.0
                                              rust-rand-core-0.9.3
                                              rust-raw-window-handle-0.6.2
                                              rust-redox-syscall-0.5.13
                                              rust-regex-1.11.1
                                              rust-regex-automata-0.4.9
                                              rust-regex-syntax-0.8.5
                                              rust-relm4-0.9.1
                                              rust-relm4-css-0.9.0
                                              rust-relm4-macros-0.9.1
                                              rust-reqwest-0.12.23
                                              rust-rfd-0.15.3
                                              rust-ring-0.17.14
                                              rust-rustc-demangle-0.1.25
                                              rust-rustc-hash-1.1.0
                                              rust-rustc-hash-2.1.1
                                              rust-rustc-version-0.4.1
                                              rust-rustix-0.38.44
                                              rust-rustix-1.0.7
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.29
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.12.0
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.4
                                              rust-rustversion-1.0.21
                                              rust-ryu-1.0.20
                                              rust-same-file-1.0.6
                                              rust-schannel-0.1.27
                                              rust-scopeguard-1.2.0
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.14.0
                                              rust-self-cell-0.10.3
                                              rust-self-cell-1.2.0
                                              rust-semver-1.0.26
                                              rust-serde-1.0.219
                                              rust-serde-derive-1.0.219
                                              rust-serde-json-1.0.140
                                              rust-serde-repr-0.1.20
                                              rust-serde-spanned-0.6.9
                                              rust-serde-spanned-1.0.0
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.6
                                              rust-sharded-slab-0.1.7
                                              rust-shlex-1.3.0
                                              rust-signal-hook-registry-1.4.5
                                              rust-simd-adler32-0.3.7
                                              rust-slab-0.4.10
                                              rust-smallvec-1.15.1
                                              rust-socket2-0.5.10
                                              rust-spin-0.9.8
                                              rust-stable-deref-trait-1.2.0
                                              rust-static-assertions-1.1.0
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.104
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-system-deps-7.0.5
                                              rust-tar-0.4.44
                                              rust-target-lexicon-0.13.2
                                              rust-tempfile-3.20.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.12
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.12
                                              rust-thread-local-1.1.9
                                              rust-time-0.3.41
                                              rust-time-core-0.1.4
                                              rust-time-macros-0.2.22
                                              rust-tinystr-0.8.1
                                              rust-tinyvec-1.10.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.46.1
                                              rust-tokio-rustls-0.26.2
                                              rust-tokio-util-0.7.16
                                              rust-toml-0.8.23
                                              rust-toml-0.9.2
                                              rust-toml-datetime-0.6.11
                                              rust-toml-datetime-0.7.0
                                              rust-toml-edit-0.22.27
                                              rust-toml-writer-1.0.0
                                              rust-tower-0.5.2
                                              rust-tower-http-0.6.6
                                              rust-tower-layer-0.3.3
                                              rust-tower-service-0.3.3
                                              rust-tracing-0.1.41
                                              rust-tracing-attributes-0.1.30
                                              rust-tracing-core-0.1.34
                                              rust-tracing-log-0.2.0
                                              rust-tracing-subscriber-0.3.19
                                              rust-try-lock-0.2.5
                                              rust-type-map-0.5.1
                                              rust-typenum-1.18.0
                                              rust-uds-windows-1.1.0
                                              rust-unic-langid-0.9.6
                                              rust-unic-langid-impl-0.9.6
                                              rust-unic-langid-macros-0.9.6
                                              rust-unic-langid-macros-impl-0.9.6
                                              rust-unicode-ident-1.0.18
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.4
                                              rust-urlencoding-2.1.3
                                              rust-utf8-iter-1.0.4
                                              rust-utf8parse-0.2.2
                                              rust-uuid-1.17.0
                                              rust-valuable-0.1.1
                                              rust-version-compare-0.2.0
                                              rust-version-check-0.9.5
                                              rust-walkdir-2.5.0
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasi-0.14.2+wasi-0.2.4
                                              rust-wasm-bindgen-0.2.100
                                              rust-wasm-bindgen-backend-0.2.100
                                              rust-wasm-bindgen-futures-0.4.50
                                              rust-wasm-bindgen-macro-0.2.100
                                              rust-wasm-bindgen-macro-support-0.2.100
                                              rust-wasm-bindgen-shared-0.2.100
                                              rust-web-sys-0.3.77
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-0.25.4
                                              rust-webpki-roots-1.0.1
                                              rust-whatadistro-0.1.0
                                              rust-which-4.4.2
                                              rust-winapi-0.3.9
                                              rust-winapi-i686-pc-windows-gnu-0.4.0
                                              rust-winapi-util-0.1.9
                                              rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                              rust-wincompatlib-0.7.7
                                              rust-windows-0.61.3
                                              rust-windows-collections-0.2.0
                                              rust-windows-core-0.61.2
                                              rust-windows-future-0.2.1
                                              rust-windows-implement-0.60.0
                                              rust-windows-interface-0.59.1
                                              rust-windows-link-0.1.3
                                              rust-windows-numerics-0.2.0
                                              rust-windows-result-0.3.4
                                              rust-windows-strings-0.4.2
                                              rust-windows-sys-0.48.0
                                              rust-windows-sys-0.52.0
                                              rust-windows-sys-0.59.0
                                              rust-windows-sys-0.60.2
                                              rust-windows-targets-0.48.5
                                              rust-windows-targets-0.52.6
                                              rust-windows-targets-0.53.2
                                              rust-windows-threading-0.1.0
                                              rust-windows-aarch64-gnullvm-0.48.5
                                              rust-windows-aarch64-gnullvm-0.52.6
                                              rust-windows-aarch64-gnullvm-0.53.0
                                              rust-windows-aarch64-msvc-0.48.5
                                              rust-windows-aarch64-msvc-0.52.6
                                              rust-windows-aarch64-msvc-0.53.0
                                              rust-windows-i686-gnu-0.48.5
                                              rust-windows-i686-gnu-0.52.6
                                              rust-windows-i686-gnu-0.53.0
                                              rust-windows-i686-gnullvm-0.52.6
                                              rust-windows-i686-gnullvm-0.53.0
                                              rust-windows-i686-msvc-0.48.5
                                              rust-windows-i686-msvc-0.52.6
                                              rust-windows-i686-msvc-0.53.0
                                              rust-windows-x86-64-gnu-0.48.5
                                              rust-windows-x86-64-gnu-0.52.6
                                              rust-windows-x86-64-gnu-0.53.0
                                              rust-windows-x86-64-gnullvm-0.48.5
                                              rust-windows-x86-64-gnullvm-0.52.6
                                              rust-windows-x86-64-gnullvm-0.53.0
                                              rust-windows-x86-64-msvc-0.48.5
                                              rust-windows-x86-64-msvc-0.52.6
                                              rust-windows-x86-64-msvc-0.53.0
                                              rust-winnow-0.7.12
                                              rust-wit-bindgen-rt-0.39.0
                                              rust-writeable-0.6.1
                                              rust-xattr-1.5.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.0
                                              rust-yoke-derive-0.8.0
                                              rust-zbus-5.8.0
                                              rust-zbus-macros-5.8.0
                                              rust-zbus-names-4.2.0
                                              rust-zerocopy-0.8.26
                                              rust-zerocopy-derive-0.8.26
                                              rust-zerofrom-0.1.6
                                              rust-zerofrom-derive-0.1.6
                                              rust-zeroize-1.8.1
                                              rust-zeroize-derive-1.4.2
                                              rust-zerotrie-0.2.2
                                              rust-zerovec-0.11.2
                                              rust-zerovec-derive-0.11.1
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.5.1
                                              rust-zopfli-0.8.2
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.15+zstd.1.5.7
                                              rust-zvariant-5.6.0
                                              rust-zvariant-derive-5.6.0
                                              rust-zvariant-utils-3.2.0))
                     (anime-game-core =>
                                      (list rust-addr2line-0.25.1
                                       rust-adler2-2.0.1
                                       rust-aes-0.8.4
                                       rust-ahash-0.8.12
                                       rust-aho-corasick-1.1.4
                                       rust-allocator-api2-0.2.21
                                       rust-anyhow-1.0.101
                                       rust-arbitrary-1.4.2
                                       rust-atomic-waker-1.1.2
                                       rust-backtrace-0.3.76
                                       rust-base64-0.21.7
                                       rust-base64-0.22.1
                                       rust-bitflags-2.10.0
                                       rust-block-buffer-0.10.4
                                       rust-bumpalo-3.19.1
                                       rust-byteorder-1.5.0
                                       rust-bytes-1.11.1
                                       rust-bzip2-0.4.4
                                       rust-bzip2-0.5.2
                                       rust-bzip2-sys-0.1.13+1.0.8
                                       rust-cached-0.55.1
                                       rust-cached-proc-macro-0.24.0
                                       rust-cached-proc-macro-types-0.1.1
                                       rust-cc-1.2.56
                                       rust-cfg-if-1.0.4
                                       rust-cfg-aliases-0.2.1
                                       rust-cipher-0.4.4
                                       rust-constant-time-eq-0.3.1
                                       rust-core-foundation-0.9.4
                                       rust-core-foundation-sys-0.8.7
                                       rust-cpufeatures-0.2.17
                                       rust-crc-3.4.0
                                       rust-crc-catalog-2.4.0
                                       rust-crc32fast-1.5.0
                                       rust-crossbeam-deque-0.8.6
                                       rust-crossbeam-epoch-0.9.18
                                       rust-crossbeam-utils-0.8.21
                                       rust-crypto-common-0.1.7
                                       rust-darling-0.20.11
                                       rust-darling-core-0.20.11
                                       rust-darling-macro-0.20.11
                                       rust-deflate64-0.1.10
                                       rust-deranged-0.5.6
                                       rust-derive-arbitrary-1.4.2
                                       rust-digest-0.10.7
                                       rust-displaydoc-0.2.5
                                       rust-dns-lookup-2.1.1
                                       rust-either-1.15.0
                                       rust-equivalent-1.0.2
                                       rust-errno-0.3.14
                                       rust-fastrand-2.3.0
                                       rust-filetime-0.2.27
                                       rust-find-msvc-tools-0.1.9
                                       rust-flate2-1.1.9
                                       rust-fnv-1.0.7
                                       rust-foldhash-0.1.5
                                       rust-form-urlencoded-1.2.2
                                       rust-fs-extra-1.3.0
                                       rust-futures-channel-0.3.31
                                       rust-futures-core-0.3.31
                                       rust-futures-io-0.3.31
                                       rust-futures-sink-0.3.31
                                       rust-futures-task-0.3.31
                                       rust-futures-util-0.3.31
                                       rust-generic-array-0.14.7
                                       rust-getrandom-0.2.17
                                       rust-getrandom-0.3.4
                                       rust-getrandom-0.4.1
                                       rust-gimli-0.32.3
                                       rust-h2-0.4.13
                                       rust-hashbrown-0.14.5
                                       rust-hashbrown-0.15.5
                                       rust-hashbrown-0.16.1
                                       rust-heck-0.5.0
                                       rust-hmac-0.12.1
                                       rust-home-0.5.12
                                       rust-http-1.4.0
                                       rust-http-body-1.0.1
                                       rust-http-body-util-0.1.3
                                       rust-httparse-1.10.1
                                       rust-hyper-1.8.1
                                       rust-hyper-rustls-0.27.7
                                       rust-hyper-util-0.1.20
                                       rust-icu-collections-2.1.1
                                       rust-icu-locale-core-2.1.1
                                       rust-icu-normalizer-2.1.1
                                       rust-icu-normalizer-data-2.1.1
                                       rust-icu-properties-2.1.2
                                       rust-icu-properties-data-2.1.2
                                       rust-icu-provider-2.1.1
                                       rust-id-arena-2.3.0
                                       rust-ident-case-1.0.1
                                       rust-idna-1.1.0
                                       rust-idna-adapter-1.2.1
                                       rust-indexmap-2.13.0
                                       rust-inout-0.1.4
                                       rust-ipnet-2.11.0
                                       rust-iri-string-0.7.10
                                       rust-itoa-1.0.17
                                       rust-jobserver-0.1.34
                                       rust-js-sys-0.3.85
                                       rust-kinda-virtual-fs-0.1.1
                                       rust-lazy-static-1.5.0
                                       rust-leb128fmt-0.1.0
                                       rust-libc-0.2.182
                                       rust-libredox-0.1.12
                                       rust-linux-raw-sys-0.4.15
                                       rust-linux-raw-sys-0.11.0
                                       rust-litemap-0.8.1
                                       rust-log-0.4.29
                                       rust-lru-slab-0.1.2
                                       rust-lzma-rs-0.3.0
                                       rust-lzma-sys-0.1.20
                                       rust-md-5-0.10.6
                                       rust-md5-asm-0.5.2
                                       rust-memchr-2.8.0
                                       rust-miniz-oxide-0.8.9
                                       rust-minreq-2.14.1
                                       rust-mio-1.1.1
                                       rust-ntapi-0.4.3
                                       rust-num-conv-0.2.0
                                       rust-objc2-core-foundation-0.3.2
                                       rust-objc2-io-kit-0.3.2
                                       rust-object-0.37.3
                                       rust-once-cell-1.21.3
                                       rust-openssl-probe-0.1.6
                                       rust-pbkdf2-0.12.2
                                       rust-percent-encoding-2.3.2
                                       rust-pin-project-lite-0.2.16
                                       rust-pin-utils-0.1.0
                                       rust-pkg-config-0.3.32
                                       rust-potential-utf-0.1.4
                                       rust-powerfmt-0.2.0
                                       rust-ppv-lite86-0.2.21
                                       rust-prettyplease-0.2.37
                                       rust-proc-macro2-1.0.106
                                       rust-protobuf-3.7.2
                                       rust-protobuf-codegen-3.7.2
                                       rust-protobuf-parse-3.7.2
                                       rust-protobuf-support-3.7.2
                                       rust-quinn-0.11.9
                                       rust-quinn-proto-0.11.13
                                       rust-quinn-udp-0.5.14
                                       rust-quote-1.0.44
                                       rust-r-efi-5.3.0
                                       rust-rand-0.9.2
                                       rust-rand-chacha-0.9.0
                                       rust-rand-core-0.9.5
                                       rust-redox-syscall-0.7.1
                                       rust-regex-1.12.3
                                       rust-regex-automata-0.4.14
                                       rust-regex-syntax-0.8.9
                                       rust-reqwest-0.12.28
                                       rust-ring-0.17.14
                                       rust-rustc-demangle-0.1.27
                                       rust-rustc-hash-2.1.1
                                       rust-rustix-0.38.44
                                       rust-rustix-1.1.3
                                       rust-rustls-0.21.12
                                       rust-rustls-0.23.36
                                       rust-rustls-native-certs-0.6.3
                                       rust-rustls-pemfile-1.0.4
                                       rust-rustls-pki-types-1.14.0
                                       rust-rustls-webpki-0.101.7
                                       rust-rustls-webpki-0.103.9
                                       rust-rustversion-1.0.22
                                       rust-ryu-1.0.23
                                       rust-schannel-0.1.28
                                       rust-sct-0.7.1
                                       rust-security-framework-2.11.1
                                       rust-security-framework-sys-2.16.0
                                       rust-semver-1.0.27
                                       rust-serde-1.0.228
                                       rust-serde-core-1.0.228
                                       rust-serde-derive-1.0.228
                                       rust-serde-json-1.0.149
                                       rust-serde-urlencoded-0.7.1
                                       rust-sha1-0.10.6
                                       rust-shlex-1.3.0
                                       rust-simd-adler32-0.3.8
                                       rust-slab-0.4.12
                                       rust-smallvec-1.15.1
                                       rust-socket2-0.6.2
                                       rust-stable-deref-trait-1.2.1
                                       rust-strsim-0.11.1
                                       rust-subtle-2.6.1
                                       rust-syn-2.0.115
                                       rust-sync-wrapper-1.0.2
                                       rust-synstructure-0.13.2
                                       rust-sysinfo-0.35.2
                                       rust-tar-0.4.44
                                       rust-tempfile-3.25.0
                                       rust-thiserror-1.0.69
                                       rust-thiserror-2.0.18
                                       rust-thiserror-impl-1.0.69
                                       rust-thiserror-impl-2.0.18
                                       rust-time-0.3.47
                                       rust-time-core-0.1.8
                                       rust-tinystr-0.8.2
                                       rust-tinyvec-1.10.0
                                       rust-tinyvec-macros-0.1.1
                                       rust-tokio-1.49.0
                                       rust-tokio-rustls-0.26.4
                                       rust-tokio-util-0.7.18
                                       rust-tower-0.5.3
                                       rust-tower-http-0.6.8
                                       rust-tower-layer-0.3.3
                                       rust-tower-service-0.3.3
                                       rust-tracing-0.1.44
                                       rust-tracing-attributes-0.1.31
                                       rust-tracing-core-0.1.36
                                       rust-try-lock-0.2.5
                                       rust-typenum-1.19.0
                                       rust-unicode-ident-1.0.23
                                       rust-unicode-xid-0.2.6
                                       rust-untrusted-0.9.0
                                       rust-url-2.5.8
                                       rust-utf8-iter-1.0.4
                                       rust-version-check-0.9.5
                                       rust-want-0.3.1
                                       rust-wasi-0.11.1+wasi-snapshot-preview1
                                       rust-wasip2-1.0.2+wasi-0.2.9
                                       rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                       rust-wasm-bindgen-0.2.108
                                       rust-wasm-bindgen-futures-0.4.58
                                       rust-wasm-bindgen-macro-0.2.108
                                       rust-wasm-bindgen-macro-support-0.2.108
                                       rust-wasm-bindgen-shared-0.2.108
                                       rust-wasm-encoder-0.244.0
                                       rust-wasm-metadata-0.244.0
                                       rust-wasmparser-0.244.0
                                       rust-web-sys-0.3.85
                                       rust-web-time-1.1.0
                                       rust-webpki-roots-1.0.6
                                       rust-which-4.4.2
                                       rust-winapi-0.3.9
                                       rust-winapi-i686-pc-windows-gnu-0.4.0
                                       rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                       rust-windows-0.61.3
                                       rust-windows-collections-0.2.0
                                       rust-windows-core-0.61.2
                                       rust-windows-future-0.2.1
                                       rust-windows-implement-0.60.2
                                       rust-windows-interface-0.59.3
                                       rust-windows-link-0.1.3
                                       rust-windows-link-0.2.1
                                       rust-windows-numerics-0.2.0
                                       rust-windows-result-0.3.4
                                       rust-windows-strings-0.4.2
                                       rust-windows-sys-0.52.0
                                       rust-windows-sys-0.59.0
                                       rust-windows-sys-0.60.2
                                       rust-windows-sys-0.61.2
                                       rust-windows-targets-0.52.6
                                       rust-windows-targets-0.53.5
                                       rust-windows-threading-0.1.0
                                       rust-windows-aarch64-gnullvm-0.52.6
                                       rust-windows-aarch64-gnullvm-0.53.1
                                       rust-windows-aarch64-msvc-0.52.6
                                       rust-windows-aarch64-msvc-0.53.1
                                       rust-windows-i686-gnu-0.52.6
                                       rust-windows-i686-gnu-0.53.1
                                       rust-windows-i686-gnullvm-0.52.6
                                       rust-windows-i686-gnullvm-0.53.1
                                       rust-windows-i686-msvc-0.52.6
                                       rust-windows-i686-msvc-0.53.1
                                       rust-windows-x86-64-gnu-0.52.6
                                       rust-windows-x86-64-gnu-0.53.1
                                       rust-windows-x86-64-gnullvm-0.52.6
                                       rust-windows-x86-64-gnullvm-0.53.1
                                       rust-windows-x86-64-msvc-0.52.6
                                       rust-windows-x86-64-msvc-0.53.1
                                       rust-wit-bindgen-0.51.0
                                       rust-wit-bindgen-core-0.51.0
                                       rust-wit-bindgen-rust-0.51.0
                                       rust-wit-bindgen-rust-macro-0.51.0
                                       rust-wit-component-0.244.0
                                       rust-wit-parser-0.244.0
                                       rust-writeable-0.6.2
                                       rust-xattr-1.6.1
                                       rust-xz-0.1.0
                                       rust-xz2-0.1.7
                                       rust-yoke-0.8.1
                                       rust-yoke-derive-0.8.1
                                       rust-zerocopy-0.8.39
                                       rust-zerocopy-derive-0.8.39
                                       rust-zerofrom-0.1.6
                                       rust-zerofrom-derive-0.1.6
                                       rust-zeroize-1.8.2
                                       rust-zeroize-derive-1.4.3
                                       rust-zerotrie-0.2.3
                                       rust-zerovec-0.11.5
                                       rust-zerovec-derive-0.11.2
                                       rust-zip-3.0.0
                                       rust-zlib-rs-0.6.0
                                       rust-zmij-1.0.21
                                       rust-zopfli-0.8.3
                                       rust-zstd-0.13.3
                                       rust-zstd-safe-7.2.4
                                       rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk =>
                                         (list rust-addr2line-0.25.1
                                          rust-adler2-2.0.1
                                          rust-aes-0.8.4
                                          rust-ahash-0.8.12
                                          rust-aho-corasick-1.1.4
                                          rust-allocator-api2-0.2.21
                                          rust-anime-game-core-1.37.2.43f2d19
                                          rust-anyhow-1.0.101
                                          rust-arbitrary-1.4.2
                                          rust-arrayref-0.3.9
                                          rust-arrayvec-0.7.6
                                          rust-atomic-waker-1.1.2
                                          rust-backtrace-0.3.76
                                          rust-base64-0.21.7
                                          rust-base64-0.22.1
                                          rust-bitflags-2.10.0
                                          rust-blake3-1.8.3
                                          rust-block-buffer-0.10.4
                                          rust-bumpalo-3.19.1
                                          rust-byteorder-1.5.0
                                          rust-bytes-1.11.1
                                          rust-bzip2-0.4.4
                                          rust-bzip2-0.5.2
                                          rust-bzip2-sys-0.1.13+1.0.8
                                          rust-cached-0.55.1
                                          rust-cached-proc-macro-0.24.0
                                          rust-cached-proc-macro-types-0.1.1
                                          rust-cc-1.2.56
                                          rust-cfg-if-1.0.4
                                          rust-cfg-aliases-0.2.1
                                          rust-cipher-0.4.4
                                          rust-constant-time-eq-0.3.1
                                          rust-constant-time-eq-0.4.2
                                          rust-core-foundation-0.9.4
                                          rust-core-foundation-sys-0.8.7
                                          rust-cpufeatures-0.2.17
                                          rust-crc-3.4.0
                                          rust-crc-catalog-2.4.0
                                          rust-crc32fast-1.5.0
                                          rust-crossbeam-deque-0.8.6
                                          rust-crossbeam-epoch-0.9.18
                                          rust-crossbeam-utils-0.8.21
                                          rust-crypto-common-0.1.7
                                          rust-darling-0.20.11
                                          rust-darling-core-0.20.11
                                          rust-darling-macro-0.20.11
                                          rust-deflate64-0.1.10
                                          rust-deranged-0.5.6
                                          rust-derive-arbitrary-1.4.2
                                          rust-digest-0.10.7
                                          rust-displaydoc-0.2.5
                                          rust-dns-lookup-2.1.1
                                          rust-either-1.15.0
                                          rust-enum-ordinalize-4.3.2
                                          rust-enum-ordinalize-derive-4.3.2
                                          rust-equivalent-1.0.2
                                          rust-errno-0.3.14
                                          rust-fastrand-2.3.0
                                          rust-filetime-0.2.27
                                          rust-find-msvc-tools-0.1.9
                                          rust-flate2-1.1.9
                                          rust-fnv-1.0.7
                                          rust-foldhash-0.1.5
                                          rust-form-urlencoded-1.2.2
                                          rust-fs-extra-1.3.0
                                          rust-futures-channel-0.3.31
                                          rust-futures-core-0.3.31
                                          rust-futures-io-0.3.31
                                          rust-futures-sink-0.3.31
                                          rust-futures-task-0.3.31
                                          rust-futures-util-0.3.31
                                          rust-generic-array-0.14.7
                                          rust-getrandom-0.2.17
                                          rust-getrandom-0.3.4
                                          rust-getrandom-0.4.1
                                          rust-gimli-0.32.3
                                          rust-h2-0.4.13
                                          rust-hashbrown-0.14.5
                                          rust-hashbrown-0.15.5
                                          rust-hashbrown-0.16.1
                                          rust-heck-0.5.0
                                          rust-hmac-0.12.1
                                          rust-home-0.5.12
                                          rust-http-1.4.0
                                          rust-http-body-1.0.1
                                          rust-http-body-util-0.1.3
                                          rust-httparse-1.10.1
                                          rust-hyper-1.8.1
                                          rust-hyper-rustls-0.27.7
                                          rust-hyper-util-0.1.20
                                          rust-icu-collections-2.1.1
                                          rust-icu-locale-core-2.1.1
                                          rust-icu-normalizer-2.1.1
                                          rust-icu-normalizer-data-2.1.1
                                          rust-icu-properties-2.1.2
                                          rust-icu-properties-data-2.1.2
                                          rust-icu-provider-2.1.1
                                          rust-id-arena-2.3.0
                                          rust-ident-case-1.0.1
                                          rust-idna-1.1.0
                                          rust-idna-adapter-1.2.1
                                          rust-indexmap-2.13.0
                                          rust-inout-0.1.4
                                          rust-ipnet-2.11.0
                                          rust-iri-string-0.7.10
                                          rust-itoa-1.0.17
                                          rust-jobserver-0.1.34
                                          rust-js-sys-0.3.85
                                          rust-kinda-virtual-fs-0.1.1
                                          rust-lazy-static-1.5.0
                                          rust-leb128fmt-0.1.0
                                          rust-libc-0.2.182
                                          rust-libredox-0.1.12
                                          rust-linux-raw-sys-0.4.15
                                          rust-linux-raw-sys-0.11.0
                                          rust-litemap-0.8.1
                                          rust-log-0.4.29
                                          rust-lru-slab-0.1.2
                                          rust-lzma-rs-0.3.0
                                          rust-lzma-sys-0.1.20
                                          rust-md-5-0.10.6
                                          rust-md5-asm-0.5.2
                                          rust-memchr-2.8.0
                                          rust-miniz-oxide-0.8.9
                                          rust-minreq-2.14.1
                                          rust-mio-1.1.1
                                          rust-ntapi-0.4.3
                                          rust-num-conv-0.2.0
                                          rust-objc2-core-foundation-0.3.2
                                          rust-objc2-io-kit-0.3.2
                                          rust-object-0.37.3
                                          rust-once-cell-1.21.3
                                          rust-openssl-probe-0.1.6
                                          rust-pbkdf2-0.12.2
                                          rust-percent-encoding-2.3.2
                                          rust-pin-project-lite-0.2.16
                                          rust-pin-utils-0.1.0
                                          rust-pkg-config-0.3.32
                                          rust-potential-utf-0.1.4
                                          rust-powerfmt-0.2.0
                                          rust-ppv-lite86-0.2.21
                                          rust-prettyplease-0.2.37
                                          rust-proc-macro2-1.0.106
                                          rust-protobuf-3.7.2
                                          rust-protobuf-codegen-3.7.2
                                          rust-protobuf-parse-3.7.2
                                          rust-protobuf-support-3.7.2
                                          rust-quinn-0.11.9
                                          rust-quinn-proto-0.11.13
                                          rust-quinn-udp-0.5.14
                                          rust-quote-1.0.44
                                          rust-r-efi-5.3.0
                                          rust-rand-0.9.2
                                          rust-rand-chacha-0.9.0
                                          rust-rand-core-0.9.5
                                          rust-redox-syscall-0.7.1
                                          rust-regex-1.12.3
                                          rust-regex-automata-0.4.14
                                          rust-regex-syntax-0.8.9
                                          rust-reqwest-0.12.28
                                          rust-ring-0.17.14
                                          rust-rustc-demangle-0.1.27
                                          rust-rustc-hash-2.1.1
                                          rust-rustix-0.38.44
                                          rust-rustix-1.1.3
                                          rust-rustls-0.21.12
                                          rust-rustls-0.23.36
                                          rust-rustls-native-certs-0.6.3
                                          rust-rustls-pemfile-1.0.4
                                          rust-rustls-pki-types-1.14.0
                                          rust-rustls-webpki-0.101.7
                                          rust-rustls-webpki-0.103.9
                                          rust-rustversion-1.0.22
                                          rust-ryu-1.0.23
                                          rust-schannel-0.1.28
                                          rust-sct-0.7.1
                                          rust-security-framework-2.11.1
                                          rust-security-framework-sys-2.16.0
                                          rust-semver-1.0.27
                                          rust-serde-1.0.228
                                          rust-serde-core-1.0.228
                                          rust-serde-derive-1.0.228
                                          rust-serde-json-1.0.149
                                          rust-serde-urlencoded-0.7.1
                                          rust-sha1-0.10.6
                                          rust-shlex-1.3.0
                                          rust-simd-adler32-0.3.8
                                          rust-slab-0.4.12
                                          rust-smallvec-1.15.1
                                          rust-socket2-0.6.2
                                          rust-stable-deref-trait-1.2.1
                                          rust-strsim-0.11.1
                                          rust-subtle-2.6.1
                                          rust-syn-2.0.115
                                          rust-sync-wrapper-1.0.2
                                          rust-synstructure-0.13.2
                                          rust-sysinfo-0.35.2
                                          rust-tar-0.4.44
                                          rust-tempfile-3.25.0
                                          rust-thiserror-1.0.69
                                          rust-thiserror-2.0.18
                                          rust-thiserror-impl-1.0.69
                                          rust-thiserror-impl-2.0.18
                                          rust-time-0.3.47
                                          rust-time-core-0.1.8
                                          rust-tinystr-0.8.2
                                          rust-tinyvec-1.10.0
                                          rust-tinyvec-macros-0.1.1
                                          rust-tokio-1.49.0
                                          rust-tokio-rustls-0.26.4
                                          rust-tokio-util-0.7.18
                                          rust-tower-0.5.3
                                          rust-tower-http-0.6.8
                                          rust-tower-layer-0.3.3
                                          rust-tower-service-0.3.3
                                          rust-tracing-0.1.44
                                          rust-tracing-attributes-0.1.31
                                          rust-tracing-core-0.1.36
                                          rust-try-lock-0.2.5
                                          rust-typenum-1.19.0
                                          rust-unicode-ident-1.0.23
                                          rust-unicode-xid-0.2.6
                                          rust-untrusted-0.9.0
                                          rust-url-2.5.8
                                          rust-utf8-iter-1.0.4
                                          rust-version-check-0.9.5
                                          rust-want-0.3.1
                                          rust-wasi-0.11.1+wasi-snapshot-preview1
                                          rust-wasip2-1.0.2+wasi-0.2.9
                                          rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                          rust-wasm-bindgen-0.2.108
                                          rust-wasm-bindgen-futures-0.4.58
                                          rust-wasm-bindgen-macro-0.2.108
                                          rust-wasm-bindgen-macro-support-0.2.108
                                          rust-wasm-bindgen-shared-0.2.108
                                          rust-wasm-encoder-0.244.0
                                          rust-wasm-metadata-0.244.0
                                          rust-wasmparser-0.244.0
                                          rust-web-sys-0.3.85
                                          rust-web-time-1.1.0
                                          rust-webpki-roots-0.25.4
                                          rust-webpki-roots-1.0.6
                                          rust-which-4.4.2
                                          rust-winapi-0.3.9
                                          rust-winapi-i686-pc-windows-gnu-0.4.0
                                          rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                          rust-wincompatlib-0.7.7
                                          rust-windows-0.61.3
                                          rust-windows-collections-0.2.0
                                          rust-windows-core-0.61.2
                                          rust-windows-future-0.2.1
                                          rust-windows-implement-0.60.2
                                          rust-windows-interface-0.59.3
                                          rust-windows-link-0.1.3
                                          rust-windows-link-0.2.1
                                          rust-windows-numerics-0.2.0
                                          rust-windows-result-0.3.4
                                          rust-windows-strings-0.4.2
                                          rust-windows-sys-0.52.0
                                          rust-windows-sys-0.59.0
                                          rust-windows-sys-0.60.2
                                          rust-windows-sys-0.61.2
                                          rust-windows-targets-0.52.6
                                          rust-windows-targets-0.53.5
                                          rust-windows-threading-0.1.0
                                          rust-windows-aarch64-gnullvm-0.52.6
                                          rust-windows-aarch64-gnullvm-0.53.1
                                          rust-windows-aarch64-msvc-0.52.6
                                          rust-windows-aarch64-msvc-0.53.1
                                          rust-windows-i686-gnu-0.52.6
                                          rust-windows-i686-gnu-0.53.1
                                          rust-windows-i686-gnullvm-0.52.6
                                          rust-windows-i686-gnullvm-0.53.1
                                          rust-windows-i686-msvc-0.52.6
                                          rust-windows-i686-msvc-0.53.1
                                          rust-windows-x86-64-gnu-0.52.6
                                          rust-windows-x86-64-gnu-0.53.1
                                          rust-windows-x86-64-gnullvm-0.52.6
                                          rust-windows-x86-64-gnullvm-0.53.1
                                          rust-windows-x86-64-msvc-0.52.6
                                          rust-windows-x86-64-msvc-0.53.1
                                          rust-wit-bindgen-0.51.0
                                          rust-wit-bindgen-core-0.51.0
                                          rust-wit-bindgen-rust-0.51.0
                                          rust-wit-bindgen-rust-macro-0.51.0
                                          rust-wit-component-0.244.0
                                          rust-wit-parser-0.244.0
                                          rust-writeable-0.6.2
                                          rust-xattr-1.6.1
                                          rust-xz-0.1.0
                                          rust-xz2-0.1.7
                                          rust-yoke-0.8.1
                                          rust-yoke-derive-0.8.1
                                          rust-zerocopy-0.8.39
                                          rust-zerocopy-derive-0.8.39
                                          rust-zerofrom-0.1.6
                                          rust-zerofrom-derive-0.1.6
                                          rust-zeroize-1.8.2
                                          rust-zeroize-derive-1.4.3
                                          rust-zerotrie-0.2.3
                                          rust-zerovec-0.11.5
                                          rust-zerovec-derive-0.11.2
                                          rust-zip-3.0.0
                                          rust-zlib-rs-0.6.0
                                          rust-zmij-1.0.21
                                          rust-zopfli-0.8.3
                                          rust-zstd-0.13.3
                                          rust-zstd-safe-7.2.4
                                          rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (honkers-launcher =>
                                       (list rust-addr2line-0.24.2
                                        rust-adler2-2.0.1
                                        rust-aes-0.8.4
                                        rust-ahash-0.8.12
                                        rust-aho-corasick-1.1.3
                                        rust-allocator-api2-0.2.21
                                        rust-anime-game-core-1.36.3.044a1e8
                                        rust-anime-launcher-sdk-1.32.0.87c4206
                                        rust-anstream-0.6.20
                                        rust-anstyle-1.0.11
                                        rust-anstyle-parse-0.2.7
                                        rust-anstyle-query-1.1.4
                                        rust-anstyle-wincon-3.0.10
                                        rust-anyhow-1.0.100
                                        rust-arbitrary-1.4.2
                                        rust-arrayref-0.3.9
                                        rust-arrayvec-0.7.6
                                        rust-ashpd-0.11.0
                                        rust-async-broadcast-0.7.2
                                        rust-async-recursion-1.1.1
                                        rust-async-trait-0.1.89
                                        rust-autocfg-1.5.0
                                        rust-backtrace-0.3.75
                                        rust-base64-0.21.7
                                        rust-base64-0.22.1
                                        rust-bitflags-2.9.4
                                        rust-blake3-1.8.2
                                        rust-block-buffer-0.10.4
                                        rust-block2-0.6.1
                                        rust-bstr-1.12.0
                                        rust-bumpalo-3.19.0
                                        rust-byteorder-1.5.0
                                        rust-bytes-1.10.1
                                        rust-bzip2-0.4.4
                                        rust-bzip2-0.5.2
                                        rust-bzip2-sys-0.1.13+1.0.8
                                        rust-cached-0.53.1
                                        rust-cached-0.55.1
                                        rust-cached-proc-macro-0.23.0
                                        rust-cached-proc-macro-0.24.0
                                        rust-cached-proc-macro-types-0.1.1
                                        rust-cairo-rs-0.20.12
                                        rust-cairo-sys-rs-0.20.10
                                        rust-cc-1.2.38
                                        rust-cfg-expr-0.20.3
                                        rust-cfg-if-1.0.3
                                        rust-cfg-aliases-0.2.1
                                        rust-cipher-0.4.4
                                        rust-colorchoice-1.0.4
                                        rust-concurrent-queue-2.5.0
                                        rust-constant-time-eq-0.3.1
                                        rust-core-foundation-0.9.4
                                        rust-core-foundation-sys-0.8.7
                                        rust-cpufeatures-0.2.17
                                        rust-crc-3.3.0
                                        rust-crc-catalog-2.4.0
                                        rust-crc32fast-1.5.0
                                        rust-crossbeam-deque-0.8.6
                                        rust-crossbeam-epoch-0.9.18
                                        rust-crossbeam-utils-0.8.21
                                        rust-crypto-common-0.1.6
                                        rust-darling-0.20.11
                                        rust-darling-core-0.20.11
                                        rust-darling-macro-0.20.11
                                        rust-deflate64-0.1.9
                                        rust-deranged-0.5.3
                                        rust-derive-arbitrary-1.4.2
                                        rust-digest-0.10.7
                                        rust-dispatch2-0.3.0
                                        rust-displaydoc-0.2.5
                                        rust-dlib-0.5.2
                                        rust-dns-lookup-2.1.1
                                        rust-downcast-rs-1.2.1
                                        rust-endi-1.1.0
                                        rust-enum-ordinalize-4.3.0
                                        rust-enum-ordinalize-derive-4.3.1
                                        rust-enumflags2-0.7.12
                                        rust-enumflags2-derive-0.7.12
                                        rust-equivalent-1.0.2
                                        rust-errno-0.3.14
                                        rust-event-listener-5.4.1
                                        rust-event-listener-strategy-0.5.4
                                        rust-fastrand-2.3.0
                                        rust-field-offset-0.3.6
                                        rust-filetime-0.2.26
                                        rust-find-msvc-tools-0.1.2
                                        rust-flate2-1.1.2
                                        rust-fluent-bundle-0.15.3
                                        rust-fluent-langneg-0.13.0
                                        rust-fluent-syntax-0.11.1
                                        rust-fluent-template-macros-0.11.0
                                        rust-fluent-templates-0.11.0
                                        rust-flume-0.11.1
                                        rust-fnv-1.0.7
                                        rust-form-urlencoded-1.2.2
                                        rust-fragile-2.0.1
                                        rust-fs-extra-1.3.0
                                        rust-futures-0.3.31
                                        rust-futures-channel-0.3.31
                                        rust-futures-core-0.3.31
                                        rust-futures-executor-0.3.31
                                        rust-futures-io-0.3.31
                                        rust-futures-lite-2.6.1
                                        rust-futures-macro-0.3.31
                                        rust-futures-sink-0.3.31
                                        rust-futures-task-0.3.31
                                        rust-futures-util-0.3.31
                                        rust-gdk-pixbuf-0.20.10
                                        rust-gdk-pixbuf-sys-0.20.10
                                        rust-gdk4-0.9.6
                                        rust-gdk4-sys-0.9.6
                                        rust-generic-array-0.14.7
                                        rust-getrandom-0.2.16
                                        rust-getrandom-0.3.3
                                        rust-gimli-0.31.1
                                        rust-gio-0.20.12
                                        rust-gio-sys-0.20.10
                                        rust-glib-0.20.12
                                        rust-glib-build-tools-0.20.0
                                        rust-glib-macros-0.20.12
                                        rust-glib-sys-0.20.10
                                        rust-globset-0.4.16
                                        rust-gobject-sys-0.20.10
                                        rust-graphene-rs-0.20.10
                                        rust-graphene-sys-0.20.10
                                        rust-gsk4-0.9.6
                                        rust-gsk4-sys-0.9.6
                                        rust-gtk4-0.9.7
                                        rust-gtk4-macros-0.9.5
                                        rust-gtk4-sys-0.9.6
                                        rust-hashbrown-0.14.5
                                        rust-hashbrown-0.16.0
                                        rust-heck-0.5.0
                                        rust-hex-0.4.3
                                        rust-hmac-0.12.1
                                        rust-human-panic-2.0.3
                                        rust-icu-collections-2.0.0
                                        rust-icu-locale-core-2.0.0
                                        rust-icu-normalizer-2.0.0
                                        rust-icu-normalizer-data-2.0.0
                                        rust-icu-properties-2.0.1
                                        rust-icu-properties-data-2.0.1
                                        rust-icu-provider-2.0.0
                                        rust-ident-case-1.0.1
                                        rust-idna-1.1.0
                                        rust-idna-adapter-1.2.1
                                        rust-ignore-0.4.23
                                        rust-indexmap-2.11.4
                                        rust-inout-0.1.4
                                        rust-intl-memoizer-0.5.3
                                        rust-intl-pluralrules-7.0.2
                                        rust-io-uring-0.7.10
                                        rust-is-docker-0.2.0
                                        rust-is-wsl-0.4.0
                                        rust-is-terminal-polyfill-1.70.1
                                        rust-itoa-1.0.15
                                        rust-jobserver-0.1.34
                                        rust-js-sys-0.3.80
                                        rust-kinda-virtual-fs-0.1.1
                                        rust-lazy-static-1.5.0
                                        rust-libadwaita-0.7.2
                                        rust-libadwaita-sys-0.7.2
                                        rust-libc-0.2.175
                                        rust-libloading-0.8.9
                                        rust-libredox-0.1.10
                                        rust-libz-rs-sys-0.5.2
                                        rust-linux-raw-sys-0.11.0
                                        rust-litemap-0.8.0
                                        rust-lock-api-0.4.13
                                        rust-log-0.4.28
                                        rust-lzma-rs-0.3.0
                                        rust-lzma-sys-0.1.20
                                        rust-md-5-0.10.6
                                        rust-md5-asm-0.5.2
                                        rust-memchr-2.7.5
                                        rust-memoffset-0.9.1
                                        rust-miniz-oxide-0.8.9
                                        rust-minreq-2.14.1
                                        rust-mio-1.0.4
                                        rust-nanorand-0.7.0
                                        rust-nix-0.30.1
                                        rust-ntapi-0.4.1
                                        rust-nu-ansi-term-0.50.1
                                        rust-num-conv-0.1.0
                                        rust-objc2-0.6.2
                                        rust-objc2-app-kit-0.3.1
                                        rust-objc2-core-foundation-0.3.1
                                        rust-objc2-encode-4.1.0
                                        rust-objc2-foundation-0.3.1
                                        rust-objc2-io-kit-0.3.1
                                        rust-object-0.36.7
                                        rust-once-cell-1.21.3
                                        rust-once-cell-polyfill-1.70.1
                                        rust-open-5.3.2
                                        rust-openssl-probe-0.1.6
                                        rust-ordered-stream-0.2.0
                                        rust-os-info-3.12.0
                                        rust-pango-0.20.12
                                        rust-pango-sys-0.20.10
                                        rust-parking-2.2.1
                                        rust-pathdiff-0.2.3
                                        rust-pbkdf2-0.12.2
                                        rust-percent-encoding-2.3.2
                                        rust-pin-project-lite-0.2.16
                                        rust-pin-utils-0.1.0
                                        rust-pkg-config-0.3.32
                                        rust-plist-1.8.0
                                        rust-pollster-0.4.0
                                        rust-potential-utf-0.1.3
                                        rust-powerfmt-0.2.0
                                        rust-ppv-lite86-0.2.21
                                        rust-proc-macro-crate-3.4.0
                                        rust-proc-macro-hack-0.5.20+deprecated
                                        rust-proc-macro2-1.0.101
                                        rust-quick-xml-0.37.5
                                        rust-quick-xml-0.38.3
                                        rust-quote-1.0.40
                                        rust-r-efi-5.3.0
                                        rust-rand-0.9.2
                                        rust-rand-chacha-0.9.0
                                        rust-rand-core-0.9.3
                                        rust-raw-window-handle-0.6.2
                                        rust-redox-syscall-0.5.17
                                        rust-regex-automata-0.4.10
                                        rust-regex-syntax-0.8.6
                                        rust-relm4-0.9.1
                                        rust-relm4-css-0.9.0
                                        rust-relm4-macros-0.9.1
                                        rust-rfd-0.15.4
                                        rust-ring-0.17.14
                                        rust-rustc-demangle-0.1.26
                                        rust-rustc-hash-1.1.0
                                        rust-rustc-hash-2.1.1
                                        rust-rustc-version-0.4.1
                                        rust-rustix-1.1.2
                                        rust-rustls-0.21.12
                                        rust-rustls-native-certs-0.6.3
                                        rust-rustls-pemfile-1.0.4
                                        rust-rustls-webpki-0.101.7
                                        rust-rustversion-1.0.22
                                        rust-ryu-1.0.20
                                        rust-same-file-1.0.6
                                        rust-schannel-0.1.28
                                        rust-scoped-tls-1.0.1
                                        rust-scopeguard-1.2.0
                                        rust-sct-0.7.1
                                        rust-security-framework-2.11.1
                                        rust-security-framework-sys-2.15.0
                                        rust-self-cell-0.10.3
                                        rust-self-cell-1.2.0
                                        rust-semver-1.0.27
                                        rust-serde-1.0.226
                                        rust-serde-core-1.0.226
                                        rust-serde-derive-1.0.226
                                        rust-serde-json-1.0.145
                                        rust-serde-repr-0.1.20
                                        rust-serde-spanned-0.6.9
                                        rust-serde-spanned-1.0.2
                                        rust-sha1-0.10.6
                                        rust-sharded-slab-0.1.7
                                        rust-shlex-1.3.0
                                        rust-signal-hook-registry-1.4.6
                                        rust-simd-adler32-0.3.7
                                        rust-slab-0.4.11
                                        rust-smallvec-1.15.1
                                        rust-socket2-0.6.0
                                        rust-spin-0.9.8
                                        rust-stable-deref-trait-1.2.0
                                        rust-static-assertions-1.1.0
                                        rust-strsim-0.11.1
                                        rust-subtle-2.6.1
                                        rust-syn-2.0.106
                                        rust-synstructure-0.13.2
                                        rust-sysinfo-0.35.2
                                        rust-system-deps-7.0.5
                                        rust-tar-0.4.44
                                        rust-target-lexicon-0.13.2
                                        rust-tempfile-3.22.0
                                        rust-thiserror-1.0.69
                                        rust-thiserror-2.0.16
                                        rust-thiserror-impl-1.0.69
                                        rust-thiserror-impl-2.0.16
                                        rust-thread-local-1.1.9
                                        rust-time-0.3.44
                                        rust-time-core-0.1.6
                                        rust-time-macros-0.2.24
                                        rust-tinystr-0.8.1
                                        rust-tokio-1.47.1
                                        rust-toml-0.8.23
                                        rust-toml-0.9.7
                                        rust-toml-datetime-0.6.11
                                        rust-toml-datetime-0.7.2
                                        rust-toml-edit-0.22.27
                                        rust-toml-edit-0.23.6
                                        rust-toml-parser-1.0.3
                                        rust-toml-writer-1.0.3
                                        rust-tracing-0.1.41
                                        rust-tracing-attributes-0.1.30
                                        rust-tracing-core-0.1.34
                                        rust-tracing-log-0.2.0
                                        rust-tracing-subscriber-0.3.20
                                        rust-type-map-0.5.1
                                        rust-typenum-1.18.0
                                        rust-uds-windows-1.1.0
                                        rust-unic-langid-0.9.6
                                        rust-unic-langid-impl-0.9.6
                                        rust-unic-langid-macros-0.9.6
                                        rust-unic-langid-macros-impl-0.9.6
                                        rust-unicode-ident-1.0.19
                                        rust-untrusted-0.9.0
                                        rust-url-2.5.7
                                        rust-urlencoding-2.1.3
                                        rust-utf8-iter-1.0.4
                                        rust-utf8parse-0.2.2
                                        rust-uuid-1.18.1
                                        rust-valuable-0.1.1
                                        rust-version-compare-0.2.0
                                        rust-version-check-0.9.5
                                        rust-walkdir-2.5.0
                                        rust-wasi-0.11.1+wasi-snapshot-preview1
                                        rust-wasi-0.14.7+wasi-0.2.4
                                        rust-wasip2-1.0.1+wasi-0.2.4
                                        rust-wasm-bindgen-0.2.103
                                        rust-wasm-bindgen-backend-0.2.103
                                        rust-wasm-bindgen-futures-0.4.53
                                        rust-wasm-bindgen-macro-0.2.103
                                        rust-wasm-bindgen-macro-support-0.2.103
                                        rust-wasm-bindgen-shared-0.2.103
                                        rust-wayland-backend-0.3.11
                                        rust-wayland-client-0.31.11
                                        rust-wayland-protocols-0.32.9
                                        rust-wayland-scanner-0.31.7
                                        rust-wayland-sys-0.31.7
                                        rust-web-sys-0.3.80
                                        rust-web-time-1.1.0
                                        rust-webpki-roots-0.25.4
                                        rust-whatadistro-0.1.0
                                        rust-winapi-0.3.9
                                        rust-winapi-i686-pc-windows-gnu-0.4.0
                                        rust-winapi-util-0.1.11
                                        rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                        rust-wincompatlib-0.7.7
                                        rust-windows-0.61.3
                                        rust-windows-collections-0.2.0
                                        rust-windows-core-0.61.2
                                        rust-windows-future-0.2.1
                                        rust-windows-implement-0.60.0
                                        rust-windows-interface-0.59.1
                                        rust-windows-link-0.1.3
                                        rust-windows-link-0.2.0
                                        rust-windows-numerics-0.2.0
                                        rust-windows-result-0.3.4
                                        rust-windows-strings-0.4.2
                                        rust-windows-sys-0.52.0
                                        rust-windows-sys-0.59.0
                                        rust-windows-sys-0.60.2
                                        rust-windows-sys-0.61.0
                                        rust-windows-targets-0.52.6
                                        rust-windows-targets-0.53.3
                                        rust-windows-threading-0.1.0
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-gnullvm-0.53.0
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-aarch64-msvc-0.53.0
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnu-0.53.0
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-gnullvm-0.53.0
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-i686-msvc-0.53.0
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnu-0.53.0
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-gnullvm-0.53.0
                                        rust-windows-x86-64-msvc-0.52.6
                                        rust-windows-x86-64-msvc-0.53.0
                                        rust-winnow-0.7.13
                                        rust-wit-bindgen-0.46.0
                                        rust-writeable-0.6.1
                                        rust-xattr-1.6.1
                                        rust-xz-0.1.0
                                        rust-xz2-0.1.7
                                        rust-yoke-0.8.0
                                        rust-yoke-derive-0.8.0
                                        rust-zbus-5.11.0
                                        rust-zbus-macros-5.11.0
                                        rust-zbus-names-4.2.0
                                        rust-zerocopy-0.8.27
                                        rust-zerocopy-derive-0.8.27
                                        rust-zerofrom-0.1.6
                                        rust-zerofrom-derive-0.1.6
                                        rust-zeroize-1.8.1
                                        rust-zeroize-derive-1.4.2
                                        rust-zerotrie-0.2.2
                                        rust-zerovec-0.11.4
                                        rust-zerovec-derive-0.11.1
                                        rust-zip-3.0.0
                                        rust-zlib-rs-0.5.2
                                        rust-zopfli-0.8.2
                                        rust-zstd-0.13.3
                                        rust-zstd-safe-7.2.4
                                        rust-zstd-sys-2.0.16+zstd.1.5.7
                                        rust-zvariant-5.7.0
                                        rust-zvariant-derive-5.7.0
                                        rust-zvariant-utils-3.2.1))
                     (sleepy-launcher =>
                                      (list rust-addr2line-0.24.2
                                       rust-adler2-2.0.1
                                       rust-aes-0.8.4
                                       rust-ahash-0.8.12
                                       rust-aho-corasick-1.1.3
                                       rust-allocator-api2-0.2.21
                                       rust-anime-game-core-1.36.3.044a1e8
                                       rust-anime-launcher-sdk-1.33.0.f75593b
                                       rust-anstream-0.6.20
                                       rust-anstyle-1.0.11
                                       rust-anstyle-parse-0.2.7
                                       rust-anstyle-query-1.1.4
                                       rust-anstyle-wincon-3.0.10
                                       rust-anyhow-1.0.100
                                       rust-arbitrary-1.4.2
                                       rust-arrayref-0.3.9
                                       rust-arrayvec-0.7.6
                                       rust-ashpd-0.11.0
                                       rust-async-broadcast-0.7.2
                                       rust-async-recursion-1.1.1
                                       rust-async-trait-0.1.89
                                       rust-autocfg-1.5.0
                                       rust-backtrace-0.3.75
                                       rust-base64-0.21.7
                                       rust-base64-0.22.1
                                       rust-bitflags-2.9.4
                                       rust-blake3-1.8.2
                                       rust-block-buffer-0.10.4
                                       rust-block2-0.6.1
                                       rust-bstr-1.12.0
                                       rust-bumpalo-3.19.0
                                       rust-byteorder-1.5.0
                                       rust-bytes-1.10.1
                                       rust-bzip2-0.4.4
                                       rust-bzip2-0.5.2
                                       rust-bzip2-sys-0.1.13+1.0.8
                                       rust-cached-0.53.1
                                       rust-cached-0.55.1
                                       rust-cached-proc-macro-0.23.0
                                       rust-cached-proc-macro-0.24.0
                                       rust-cached-proc-macro-types-0.1.1
                                       rust-cairo-rs-0.20.12
                                       rust-cairo-sys-rs-0.20.10
                                       rust-cc-1.2.38
                                       rust-cfg-expr-0.20.3
                                       rust-cfg-if-1.0.3
                                       rust-cfg-aliases-0.2.1
                                       rust-cipher-0.4.4
                                       rust-colorchoice-1.0.4
                                       rust-concurrent-queue-2.5.0
                                       rust-constant-time-eq-0.3.1
                                       rust-core-foundation-0.9.4
                                       rust-core-foundation-sys-0.8.7
                                       rust-cpufeatures-0.2.17
                                       rust-crc-3.3.0
                                       rust-crc-catalog-2.4.0
                                       rust-crc32fast-1.5.0
                                       rust-crossbeam-deque-0.8.6
                                       rust-crossbeam-epoch-0.9.18
                                       rust-crossbeam-utils-0.8.21
                                       rust-crypto-common-0.1.6
                                       rust-darling-0.20.11
                                       rust-darling-core-0.20.11
                                       rust-darling-macro-0.20.11
                                       rust-deflate64-0.1.9
                                       rust-deranged-0.5.3
                                       rust-derive-arbitrary-1.4.2
                                       rust-digest-0.10.7
                                       rust-dispatch2-0.3.0
                                       rust-displaydoc-0.2.5
                                       rust-dlib-0.5.2
                                       rust-dns-lookup-2.1.1
                                       rust-downcast-rs-1.2.1
                                       rust-endi-1.1.0
                                       rust-enum-ordinalize-4.3.0
                                       rust-enum-ordinalize-derive-4.3.1
                                       rust-enumflags2-0.7.12
                                       rust-enumflags2-derive-0.7.12
                                       rust-equivalent-1.0.2
                                       rust-errno-0.3.14
                                       rust-event-listener-5.4.1
                                       rust-event-listener-strategy-0.5.4
                                       rust-fastrand-2.3.0
                                       rust-field-offset-0.3.6
                                       rust-filetime-0.2.26
                                       rust-find-msvc-tools-0.1.2
                                       rust-flate2-1.1.2
                                       rust-fluent-bundle-0.15.3
                                       rust-fluent-langneg-0.13.0
                                       rust-fluent-syntax-0.11.1
                                       rust-fluent-template-macros-0.11.0
                                       rust-fluent-templates-0.11.0
                                       rust-flume-0.11.1
                                       rust-fnv-1.0.7
                                       rust-form-urlencoded-1.2.2
                                       rust-fragile-2.0.1
                                       rust-fs-extra-1.3.0
                                       rust-futures-0.3.31
                                       rust-futures-channel-0.3.31
                                       rust-futures-core-0.3.31
                                       rust-futures-executor-0.3.31
                                       rust-futures-io-0.3.31
                                       rust-futures-lite-2.6.1
                                       rust-futures-macro-0.3.31
                                       rust-futures-sink-0.3.31
                                       rust-futures-task-0.3.31
                                       rust-futures-util-0.3.31
                                       rust-gdk-pixbuf-0.20.10
                                       rust-gdk-pixbuf-sys-0.20.10
                                       rust-gdk4-0.9.6
                                       rust-gdk4-sys-0.9.6
                                       rust-generic-array-0.14.7
                                       rust-getrandom-0.2.16
                                       rust-getrandom-0.3.3
                                       rust-gimli-0.31.1
                                       rust-gio-0.20.12
                                       rust-gio-sys-0.20.10
                                       rust-glib-0.20.12
                                       rust-glib-build-tools-0.20.0
                                       rust-glib-macros-0.20.12
                                       rust-glib-sys-0.20.10
                                       rust-globset-0.4.16
                                       rust-gobject-sys-0.20.10
                                       rust-graphene-rs-0.20.10
                                       rust-graphene-sys-0.20.10
                                       rust-gsk4-0.9.6
                                       rust-gsk4-sys-0.9.6
                                       rust-gtk4-0.9.7
                                       rust-gtk4-macros-0.9.5
                                       rust-gtk4-sys-0.9.6
                                       rust-hashbrown-0.14.5
                                       rust-hashbrown-0.16.0
                                       rust-heck-0.5.0
                                       rust-hex-0.4.3
                                       rust-hmac-0.12.1
                                       rust-human-panic-2.0.3
                                       rust-icu-collections-2.0.0
                                       rust-icu-locale-core-2.0.0
                                       rust-icu-normalizer-2.0.0
                                       rust-icu-normalizer-data-2.0.0
                                       rust-icu-properties-2.0.1
                                       rust-icu-properties-data-2.0.1
                                       rust-icu-provider-2.0.0
                                       rust-ident-case-1.0.1
                                       rust-idna-1.1.0
                                       rust-idna-adapter-1.2.1
                                       rust-ignore-0.4.23
                                       rust-indexmap-2.11.4
                                       rust-inout-0.1.4
                                       rust-intl-memoizer-0.5.3
                                       rust-intl-pluralrules-7.0.2
                                       rust-io-uring-0.7.10
                                       rust-is-docker-0.2.0
                                       rust-is-wsl-0.4.0
                                       rust-is-terminal-polyfill-1.70.1
                                       rust-itoa-1.0.15
                                       rust-jobserver-0.1.34
                                       rust-js-sys-0.3.80
                                       rust-kinda-virtual-fs-0.1.1
                                       rust-lazy-static-1.5.0
                                       rust-libadwaita-0.7.2
                                       rust-libadwaita-sys-0.7.2
                                       rust-libc-0.2.175
                                       rust-libloading-0.8.9
                                       rust-libredox-0.1.10
                                       rust-libz-rs-sys-0.5.2
                                       rust-linux-raw-sys-0.11.0
                                       rust-litemap-0.8.0
                                       rust-lock-api-0.4.13
                                       rust-log-0.4.28
                                       rust-lzma-rs-0.3.0
                                       rust-lzma-sys-0.1.20
                                       rust-md-5-0.10.6
                                       rust-md5-asm-0.5.2
                                       rust-memchr-2.7.5
                                       rust-memoffset-0.9.1
                                       rust-miniz-oxide-0.8.9
                                       rust-minreq-2.14.1
                                       rust-mio-1.0.4
                                       rust-nanorand-0.7.0
                                       rust-nix-0.30.1
                                       rust-ntapi-0.4.1
                                       rust-nu-ansi-term-0.50.1
                                       rust-num-conv-0.1.0
                                       rust-objc2-0.6.2
                                       rust-objc2-app-kit-0.3.1
                                       rust-objc2-core-foundation-0.3.1
                                       rust-objc2-encode-4.1.0
                                       rust-objc2-foundation-0.3.1
                                       rust-objc2-io-kit-0.3.1
                                       rust-object-0.36.7
                                       rust-once-cell-1.21.3
                                       rust-once-cell-polyfill-1.70.1
                                       rust-open-5.3.2
                                       rust-openssl-probe-0.1.6
                                       rust-ordered-stream-0.2.0
                                       rust-os-info-3.12.0
                                       rust-pango-0.20.12
                                       rust-pango-sys-0.20.10
                                       rust-parking-2.2.1
                                       rust-pathdiff-0.2.3
                                       rust-pbkdf2-0.12.2
                                       rust-percent-encoding-2.3.2
                                       rust-pin-project-lite-0.2.16
                                       rust-pin-utils-0.1.0
                                       rust-pkg-config-0.3.32
                                       rust-plist-1.8.0
                                       rust-pollster-0.4.0
                                       rust-potential-utf-0.1.3
                                       rust-powerfmt-0.2.0
                                       rust-ppv-lite86-0.2.21
                                       rust-proc-macro-crate-3.4.0
                                       rust-proc-macro-hack-0.5.20+deprecated
                                       rust-proc-macro2-1.0.101
                                       rust-quick-xml-0.37.5
                                       rust-quick-xml-0.38.3
                                       rust-quote-1.0.40
                                       rust-r-efi-5.3.0
                                       rust-rand-0.9.2
                                       rust-rand-chacha-0.9.0
                                       rust-rand-core-0.9.3
                                       rust-raw-window-handle-0.6.2
                                       rust-redox-syscall-0.5.17
                                       rust-regex-automata-0.4.10
                                       rust-regex-syntax-0.8.6
                                       rust-relm4-0.9.1
                                       rust-relm4-css-0.9.0
                                       rust-relm4-macros-0.9.1
                                       rust-rfd-0.15.4
                                       rust-ring-0.17.14
                                       rust-rustc-demangle-0.1.26
                                       rust-rustc-hash-1.1.0
                                       rust-rustc-hash-2.1.1
                                       rust-rustc-version-0.4.1
                                       rust-rustix-1.1.2
                                       rust-rustls-0.21.12
                                       rust-rustls-native-certs-0.6.3
                                       rust-rustls-pemfile-1.0.4
                                       rust-rustls-webpki-0.101.7
                                       rust-rustversion-1.0.22
                                       rust-ryu-1.0.20
                                       rust-same-file-1.0.6
                                       rust-schannel-0.1.28
                                       rust-scoped-tls-1.0.1
                                       rust-scopeguard-1.2.0
                                       rust-sct-0.7.1
                                       rust-security-framework-2.11.1
                                       rust-security-framework-sys-2.15.0
                                       rust-self-cell-0.10.3
                                       rust-self-cell-1.2.0
                                       rust-semver-1.0.27
                                       rust-serde-1.0.226
                                       rust-serde-core-1.0.226
                                       rust-serde-derive-1.0.226
                                       rust-serde-json-1.0.145
                                       rust-serde-repr-0.1.20
                                       rust-serde-spanned-0.6.9
                                       rust-serde-spanned-1.0.2
                                       rust-sha1-0.10.6
                                       rust-sharded-slab-0.1.7
                                       rust-shlex-1.3.0
                                       rust-signal-hook-registry-1.4.6
                                       rust-simd-adler32-0.3.7
                                       rust-slab-0.4.11
                                       rust-smallvec-1.15.1
                                       rust-socket2-0.6.0
                                       rust-spin-0.9.8
                                       rust-stable-deref-trait-1.2.0
                                       rust-static-assertions-1.1.0
                                       rust-strsim-0.11.1
                                       rust-subtle-2.6.1
                                       rust-syn-2.0.106
                                       rust-synstructure-0.13.2
                                       rust-sysinfo-0.35.2
                                       rust-system-deps-7.0.5
                                       rust-tar-0.4.44
                                       rust-target-lexicon-0.13.2
                                       rust-tempfile-3.22.0
                                       rust-thiserror-1.0.69
                                       rust-thiserror-2.0.16
                                       rust-thiserror-impl-1.0.69
                                       rust-thiserror-impl-2.0.16
                                       rust-thread-local-1.1.9
                                       rust-time-0.3.44
                                       rust-time-core-0.1.6
                                       rust-time-macros-0.2.24
                                       rust-tinystr-0.8.1
                                       rust-tokio-1.47.1
                                       rust-toml-0.8.23
                                       rust-toml-0.9.7
                                       rust-toml-datetime-0.6.11
                                       rust-toml-datetime-0.7.2
                                       rust-toml-edit-0.22.27
                                       rust-toml-edit-0.23.6
                                       rust-toml-parser-1.0.3
                                       rust-toml-writer-1.0.3
                                       rust-tracing-0.1.41
                                       rust-tracing-attributes-0.1.30
                                       rust-tracing-core-0.1.34
                                       rust-tracing-log-0.2.0
                                       rust-tracing-subscriber-0.3.20
                                       rust-type-map-0.5.1
                                       rust-typenum-1.18.0
                                       rust-uds-windows-1.1.0
                                       rust-unic-langid-0.9.6
                                       rust-unic-langid-impl-0.9.6
                                       rust-unic-langid-macros-0.9.6
                                       rust-unic-langid-macros-impl-0.9.6
                                       rust-unicode-ident-1.0.19
                                       rust-untrusted-0.9.0
                                       rust-url-2.5.7
                                       rust-urlencoding-2.1.3
                                       rust-utf8-iter-1.0.4
                                       rust-utf8parse-0.2.2
                                       rust-uuid-1.18.1
                                       rust-valuable-0.1.1
                                       rust-version-compare-0.2.0
                                       rust-version-check-0.9.5
                                       rust-walkdir-2.5.0
                                       rust-wasi-0.11.1+wasi-snapshot-preview1
                                       rust-wasi-0.14.7+wasi-0.2.4
                                       rust-wasip2-1.0.1+wasi-0.2.4
                                       rust-wasm-bindgen-0.2.103
                                       rust-wasm-bindgen-backend-0.2.103
                                       rust-wasm-bindgen-futures-0.4.53
                                       rust-wasm-bindgen-macro-0.2.103
                                       rust-wasm-bindgen-macro-support-0.2.103
                                       rust-wasm-bindgen-shared-0.2.103
                                       rust-wayland-backend-0.3.11
                                       rust-wayland-client-0.31.11
                                       rust-wayland-protocols-0.32.9
                                       rust-wayland-scanner-0.31.7
                                       rust-wayland-sys-0.31.7
                                       rust-web-sys-0.3.80
                                       rust-web-time-1.1.0
                                       rust-webpki-roots-0.25.4
                                       rust-whatadistro-0.1.0
                                       rust-winapi-0.3.9
                                       rust-winapi-i686-pc-windows-gnu-0.4.0
                                       rust-winapi-util-0.1.11
                                       rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                       rust-wincompatlib-0.7.7
                                       rust-windows-0.61.3
                                       rust-windows-collections-0.2.0
                                       rust-windows-core-0.61.2
                                       rust-windows-future-0.2.1
                                       rust-windows-implement-0.60.0
                                       rust-windows-interface-0.59.1
                                       rust-windows-link-0.1.3
                                       rust-windows-link-0.2.0
                                       rust-windows-numerics-0.2.0
                                       rust-windows-result-0.3.4
                                       rust-windows-strings-0.4.2
                                       rust-windows-sys-0.52.0
                                       rust-windows-sys-0.59.0
                                       rust-windows-sys-0.60.2
                                       rust-windows-sys-0.61.0
                                       rust-windows-targets-0.52.6
                                       rust-windows-targets-0.53.3
                                       rust-windows-threading-0.1.0
                                       rust-windows-aarch64-gnullvm-0.52.6
                                       rust-windows-aarch64-gnullvm-0.53.0
                                       rust-windows-aarch64-msvc-0.52.6
                                       rust-windows-aarch64-msvc-0.53.0
                                       rust-windows-i686-gnu-0.52.6
                                       rust-windows-i686-gnu-0.53.0
                                       rust-windows-i686-gnullvm-0.52.6
                                       rust-windows-i686-gnullvm-0.53.0
                                       rust-windows-i686-msvc-0.52.6
                                       rust-windows-i686-msvc-0.53.0
                                       rust-windows-x86-64-gnu-0.52.6
                                       rust-windows-x86-64-gnu-0.53.0
                                       rust-windows-x86-64-gnullvm-0.52.6
                                       rust-windows-x86-64-gnullvm-0.53.0
                                       rust-windows-x86-64-msvc-0.52.6
                                       rust-windows-x86-64-msvc-0.53.0
                                       rust-winnow-0.7.13
                                       rust-wit-bindgen-0.46.0
                                       rust-writeable-0.6.1
                                       rust-xattr-1.6.1
                                       rust-xz-0.1.0
                                       rust-xz2-0.1.7
                                       rust-yoke-0.8.0
                                       rust-yoke-derive-0.8.0
                                       rust-zbus-5.11.0
                                       rust-zbus-macros-5.11.0
                                       rust-zbus-names-4.2.0
                                       rust-zerocopy-0.8.27
                                       rust-zerocopy-derive-0.8.27
                                       rust-zerofrom-0.1.6
                                       rust-zerofrom-derive-0.1.6
                                       rust-zeroize-1.8.1
                                       rust-zeroize-derive-1.4.2
                                       rust-zerotrie-0.2.2
                                       rust-zerovec-0.11.4
                                       rust-zerovec-derive-0.11.1
                                       rust-zip-3.0.0
                                       rust-zlib-rs-0.5.2
                                       rust-zopfli-0.8.2
                                       rust-zstd-0.13.3
                                       rust-zstd-safe-7.2.4
                                       rust-zstd-sys-2.0.16+zstd.1.5.7
                                       rust-zvariant-5.7.0
                                       rust-zvariant-derive-5.7.0
                                       rust-zvariant-utils-3.2.1))
                     (the-honkers-railway-launcher =>
                                                   (list rust-addr2line-0.24.2
                                                    rust-adler2-2.0.1
                                                    rust-aes-0.8.4
                                                    rust-ahash-0.8.12
                                                    rust-aho-corasick-1.1.3
                                                    rust-allocator-api2-0.2.21
                                                    rust-anime-game-core-1.37.2.43f2d19
                                                    rust-anime-launcher-sdk-1.34.3.04a6649
                                                    rust-anstream-0.6.20
                                                    rust-anstyle-1.0.11
                                                    rust-anstyle-parse-0.2.7
                                                    rust-anstyle-query-1.1.4
                                                    rust-anstyle-wincon-3.0.10
                                                    rust-anyhow-1.0.100
                                                    rust-arbitrary-1.4.2
                                                    rust-arrayref-0.3.9
                                                    rust-arrayvec-0.7.6
                                                    rust-ashpd-0.11.0
                                                    rust-async-broadcast-0.7.2
                                                    rust-async-recursion-1.1.1
                                                    rust-async-trait-0.1.89
                                                    rust-atomic-waker-1.1.2
                                                    rust-autocfg-1.5.0
                                                    rust-backtrace-0.3.75
                                                    rust-base64-0.21.7
                                                    rust-base64-0.22.1
                                                    rust-bitflags-2.9.4
                                                    rust-blake3-1.8.2
                                                    rust-block-buffer-0.10.4
                                                    rust-block2-0.6.1
                                                    rust-bstr-1.12.0
                                                    rust-bumpalo-3.19.0
                                                    rust-byteorder-1.5.0
                                                    rust-bytes-1.10.1
                                                    rust-bzip2-0.4.4
                                                    rust-bzip2-0.5.2
                                                    rust-bzip2-sys-0.1.13+1.0.8
                                                    rust-cached-0.55.1
                                                    rust-cached-proc-macro-0.24.0
                                                    rust-cached-proc-macro-types-0.1.1
                                                    rust-cairo-rs-0.20.12
                                                    rust-cairo-sys-rs-0.20.10
                                                    rust-cc-1.2.38
                                                    rust-cfg-expr-0.20.3
                                                    rust-cfg-if-1.0.3
                                                    rust-cfg-aliases-0.2.1
                                                    rust-cipher-0.4.4
                                                    rust-colorchoice-1.0.4
                                                    rust-concurrent-queue-2.5.0
                                                    rust-constant-time-eq-0.3.1
                                                    rust-core-foundation-0.9.4
                                                    rust-core-foundation-sys-0.8.7
                                                    rust-cpufeatures-0.2.17
                                                    rust-crc-3.3.0
                                                    rust-crc-catalog-2.4.0
                                                    rust-crc32fast-1.5.0
                                                    rust-crossbeam-deque-0.8.6
                                                    rust-crossbeam-epoch-0.9.18
                                                    rust-crossbeam-utils-0.8.21
                                                    rust-crypto-common-0.1.6
                                                    rust-darling-0.20.11
                                                    rust-darling-core-0.20.11
                                                    rust-darling-macro-0.20.11
                                                    rust-deflate64-0.1.9
                                                    rust-deranged-0.5.4
                                                    rust-derive-arbitrary-1.4.2
                                                    rust-digest-0.10.7
                                                    rust-dispatch2-0.3.0
                                                    rust-displaydoc-0.2.5
                                                    rust-dlib-0.5.2
                                                    rust-dns-lookup-2.1.1
                                                    rust-downcast-rs-1.2.1
                                                    rust-either-1.15.0
                                                    rust-endi-1.1.0
                                                    rust-enum-ordinalize-4.3.0
                                                    rust-enum-ordinalize-derive-4.3.1
                                                    rust-enumflags2-0.7.12
                                                    rust-enumflags2-derive-0.7.12
                                                    rust-equivalent-1.0.2
                                                    rust-errno-0.3.14
                                                    rust-event-listener-5.4.1
                                                    rust-event-listener-strategy-0.5.4
                                                    rust-fastrand-2.3.0
                                                    rust-field-offset-0.3.6
                                                    rust-filetime-0.2.26
                                                    rust-find-msvc-tools-0.1.2
                                                    rust-flate2-1.1.2
                                                    rust-fluent-bundle-0.16.0
                                                    rust-fluent-langneg-0.13.0
                                                    rust-fluent-syntax-0.12.0
                                                    rust-fluent-template-macros-0.13.1
                                                    rust-fluent-templates-0.13.1
                                                    rust-flume-0.11.1
                                                    rust-fnv-1.0.7
                                                    rust-form-urlencoded-1.2.2
                                                    rust-fragile-2.0.1
                                                    rust-fs-extra-1.3.0
                                                    rust-futures-0.3.31
                                                    rust-futures-channel-0.3.31
                                                    rust-futures-core-0.3.31
                                                    rust-futures-executor-0.3.31
                                                    rust-futures-io-0.3.31
                                                    rust-futures-lite-2.6.1
                                                    rust-futures-macro-0.3.31
                                                    rust-futures-sink-0.3.31
                                                    rust-futures-task-0.3.31
                                                    rust-futures-util-0.3.31
                                                    rust-gdk-pixbuf-0.20.10
                                                    rust-gdk-pixbuf-sys-0.20.10
                                                    rust-gdk4-0.9.6
                                                    rust-gdk4-sys-0.9.6
                                                    rust-generic-array-0.14.7
                                                    rust-getrandom-0.2.16
                                                    rust-getrandom-0.3.3
                                                    rust-gimli-0.31.1
                                                    rust-gio-0.20.12
                                                    rust-gio-sys-0.20.10
                                                    rust-glib-0.20.12
                                                    rust-glib-build-tools-0.20.0
                                                    rust-glib-macros-0.20.12
                                                    rust-glib-sys-0.20.10
                                                    rust-globset-0.4.16
                                                    rust-gobject-sys-0.20.10
                                                    rust-graphene-rs-0.20.10
                                                    rust-graphene-sys-0.20.10
                                                    rust-gsk4-0.9.6
                                                    rust-gsk4-sys-0.9.6
                                                    rust-gtk4-0.9.7
                                                    rust-gtk4-macros-0.9.5
                                                    rust-gtk4-sys-0.9.6
                                                    rust-h2-0.4.12
                                                    rust-hashbrown-0.14.5
                                                    rust-hashbrown-0.16.0
                                                    rust-heck-0.5.0
                                                    rust-hex-0.4.3
                                                    rust-hmac-0.12.1
                                                    rust-home-0.5.12
                                                    rust-http-1.4.0
                                                    rust-http-body-1.0.1
                                                    rust-http-body-util-0.1.3
                                                    rust-httparse-1.10.1
                                                    rust-human-panic-2.0.3
                                                    rust-hyper-1.8.1
                                                    rust-hyper-rustls-0.27.7
                                                    rust-hyper-util-0.1.19
                                                    rust-icu-collections-2.0.0
                                                    rust-icu-locale-core-2.0.0
                                                    rust-icu-normalizer-2.0.0
                                                    rust-icu-normalizer-data-2.0.0
                                                    rust-icu-properties-2.0.1
                                                    rust-icu-properties-data-2.0.1
                                                    rust-icu-provider-2.0.0
                                                    rust-ident-case-1.0.1
                                                    rust-idna-1.1.0
                                                    rust-idna-adapter-1.2.1
                                                    rust-ignore-0.4.23
                                                    rust-indexmap-2.11.4
                                                    rust-inout-0.1.4
                                                    rust-intl-memoizer-0.5.3
                                                    rust-intl-pluralrules-7.0.2
                                                    rust-io-uring-0.7.10
                                                    rust-ipnet-2.11.0
                                                    rust-iri-string-0.7.9
                                                    rust-is-docker-0.2.0
                                                    rust-is-wsl-0.4.0
                                                    rust-is-terminal-polyfill-1.70.1
                                                    rust-itoa-1.0.15
                                                    rust-jobserver-0.1.34
                                                    rust-js-sys-0.3.81
                                                    rust-kinda-virtual-fs-0.1.1
                                                    rust-lazy-static-1.5.0
                                                    rust-libadwaita-0.7.2
                                                    rust-libadwaita-sys-0.7.2
                                                    rust-libc-0.2.176
                                                    rust-libloading-0.8.9
                                                    rust-libredox-0.1.10
                                                    rust-libz-rs-sys-0.5.2
                                                    rust-linux-raw-sys-0.4.15
                                                    rust-linux-raw-sys-0.11.0
                                                    rust-litemap-0.8.0
                                                    rust-lock-api-0.4.13
                                                    rust-log-0.4.28
                                                    rust-lru-slab-0.1.2
                                                    rust-lzma-rs-0.3.0
                                                    rust-lzma-sys-0.1.20
                                                    rust-md-5-0.10.6
                                                    rust-md5-asm-0.5.2
                                                    rust-memchr-2.7.6
                                                    rust-memoffset-0.9.1
                                                    rust-miniz-oxide-0.8.9
                                                    rust-minreq-2.14.1
                                                    rust-mio-1.0.4
                                                    rust-nanorand-0.7.0
                                                    rust-nix-0.30.1
                                                    rust-ntapi-0.4.1
                                                    rust-nu-ansi-term-0.50.1
                                                    rust-num-conv-0.1.0
                                                    rust-objc2-0.6.2
                                                    rust-objc2-app-kit-0.3.1
                                                    rust-objc2-core-foundation-0.3.1
                                                    rust-objc2-encode-4.1.0
                                                    rust-objc2-foundation-0.3.1
                                                    rust-objc2-io-kit-0.3.1
                                                    rust-object-0.36.7
                                                    rust-once-cell-1.21.3
                                                    rust-once-cell-polyfill-1.70.1
                                                    rust-open-5.3.2
                                                    rust-openssl-probe-0.1.6
                                                    rust-ordered-stream-0.2.0
                                                    rust-os-info-3.12.0
                                                    rust-pango-0.20.12
                                                    rust-pango-sys-0.20.10
                                                    rust-parking-2.2.1
                                                    rust-pathdiff-0.2.3
                                                    rust-pbkdf2-0.12.2
                                                    rust-percent-encoding-2.3.2
                                                    rust-pin-project-lite-0.2.16
                                                    rust-pin-utils-0.1.0
                                                    rust-pkg-config-0.3.32
                                                    rust-plist-1.8.0
                                                    rust-pollster-0.4.0
                                                    rust-potential-utf-0.1.3
                                                    rust-powerfmt-0.2.0
                                                    rust-ppv-lite86-0.2.21
                                                    rust-proc-macro-crate-3.4.0
                                                    rust-proc-macro-hack-0.5.20+deprecated
                                                    rust-proc-macro2-1.0.101
                                                    rust-protobuf-3.7.2
                                                    rust-protobuf-codegen-3.7.2
                                                    rust-protobuf-parse-3.7.2
                                                    rust-protobuf-support-3.7.2
                                                    rust-quick-xml-0.37.5
                                                    rust-quick-xml-0.38.3
                                                    rust-quinn-0.11.9
                                                    rust-quinn-proto-0.11.13
                                                    rust-quinn-udp-0.5.14
                                                    rust-quote-1.0.40
                                                    rust-r-efi-5.3.0
                                                    rust-rand-0.9.2
                                                    rust-rand-chacha-0.9.0
                                                    rust-rand-core-0.9.3
                                                    rust-raw-window-handle-0.6.2
                                                    rust-redox-syscall-0.5.17
                                                    rust-regex-1.11.3
                                                    rust-regex-automata-0.4.11
                                                    rust-regex-syntax-0.8.6
                                                    rust-relm4-0.9.1
                                                    rust-relm4-css-0.9.0
                                                    rust-relm4-macros-0.9.1
                                                    rust-reqwest-0.12.26
                                                    rust-rfd-0.15.4
                                                    rust-ring-0.17.14
                                                    rust-rustc-demangle-0.1.26
                                                    rust-rustc-hash-2.1.1
                                                    rust-rustc-version-0.4.1
                                                    rust-rustix-0.38.44
                                                    rust-rustix-1.1.2
                                                    rust-rustls-0.21.12
                                                    rust-rustls-0.23.35
                                                    rust-rustls-native-certs-0.6.3
                                                    rust-rustls-pemfile-1.0.4
                                                    rust-rustls-pki-types-1.13.1
                                                    rust-rustls-webpki-0.101.7
                                                    rust-rustls-webpki-0.103.8
                                                    rust-rustversion-1.0.22
                                                    rust-ryu-1.0.20
                                                    rust-same-file-1.0.6
                                                    rust-schannel-0.1.28
                                                    rust-scoped-tls-1.0.1
                                                    rust-scopeguard-1.2.0
                                                    rust-sct-0.7.1
                                                    rust-security-framework-2.11.1
                                                    rust-security-framework-sys-2.15.0
                                                    rust-self-cell-1.2.0
                                                    rust-semver-1.0.27
                                                    rust-serde-1.0.226
                                                    rust-serde-core-1.0.226
                                                    rust-serde-derive-1.0.226
                                                    rust-serde-json-1.0.145
                                                    rust-serde-repr-0.1.20
                                                    rust-serde-spanned-0.6.9
                                                    rust-serde-spanned-1.0.2
                                                    rust-serde-urlencoded-0.7.1
                                                    rust-sha1-0.10.6
                                                    rust-sharded-slab-0.1.7
                                                    rust-shlex-1.3.0
                                                    rust-signal-hook-registry-1.4.6
                                                    rust-simd-adler32-0.3.7
                                                    rust-slab-0.4.11
                                                    rust-smallvec-1.15.1
                                                    rust-socket2-0.6.0
                                                    rust-spin-0.9.8
                                                    rust-stable-deref-trait-1.2.0
                                                    rust-static-assertions-1.1.0
                                                    rust-strsim-0.11.1
                                                    rust-subtle-2.6.1
                                                    rust-syn-2.0.106
                                                    rust-sync-wrapper-1.0.2
                                                    rust-synstructure-0.13.2
                                                    rust-sysinfo-0.35.2
                                                    rust-system-deps-7.0.5
                                                    rust-tar-0.4.44
                                                    rust-target-lexicon-0.13.2
                                                    rust-tempfile-3.23.0
                                                    rust-thiserror-1.0.69
                                                    rust-thiserror-2.0.16
                                                    rust-thiserror-impl-1.0.69
                                                    rust-thiserror-impl-2.0.16
                                                    rust-thread-local-1.1.9
                                                    rust-time-0.3.44
                                                    rust-time-core-0.1.6
                                                    rust-time-macros-0.2.24
                                                    rust-tinystr-0.8.1
                                                    rust-tinyvec-1.10.0
                                                    rust-tinyvec-macros-0.1.1
                                                    rust-tokio-1.47.1
                                                    rust-tokio-rustls-0.26.4
                                                    rust-tokio-util-0.7.17
                                                    rust-toml-0.8.23
                                                    rust-toml-0.9.7
                                                    rust-toml-datetime-0.6.11
                                                    rust-toml-datetime-0.7.2
                                                    rust-toml-edit-0.22.27
                                                    rust-toml-edit-0.23.6
                                                    rust-toml-parser-1.0.3
                                                    rust-toml-writer-1.0.3
                                                    rust-tower-0.5.2
                                                    rust-tower-http-0.6.8
                                                    rust-tower-layer-0.3.3
                                                    rust-tower-service-0.3.3
                                                    rust-tracing-0.1.41
                                                    rust-tracing-attributes-0.1.30
                                                    rust-tracing-core-0.1.34
                                                    rust-tracing-log-0.2.0
                                                    rust-tracing-subscriber-0.3.20
                                                    rust-try-lock-0.2.5
                                                    rust-type-map-0.5.1
                                                    rust-typenum-1.18.0
                                                    rust-uds-windows-1.1.0
                                                    rust-unic-langid-0.9.6
                                                    rust-unic-langid-impl-0.9.6
                                                    rust-unic-langid-macros-0.9.6
                                                    rust-unic-langid-macros-impl-0.9.6
                                                    rust-unicode-ident-1.0.19
                                                    rust-untrusted-0.9.0
                                                    rust-url-2.5.7
                                                    rust-urlencoding-2.1.3
                                                    rust-utf8-iter-1.0.4
                                                    rust-utf8parse-0.2.2
                                                    rust-uuid-1.18.1
                                                    rust-valuable-0.1.1
                                                    rust-version-compare-0.2.0
                                                    rust-version-check-0.9.5
                                                    rust-walkdir-2.5.0
                                                    rust-want-0.3.1
                                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                                    rust-wasi-0.14.7+wasi-0.2.4
                                                    rust-wasip2-1.0.1+wasi-0.2.4
                                                    rust-wasm-bindgen-0.2.104
                                                    rust-wasm-bindgen-backend-0.2.104
                                                    rust-wasm-bindgen-futures-0.4.54
                                                    rust-wasm-bindgen-macro-0.2.104
                                                    rust-wasm-bindgen-macro-support-0.2.104
                                                    rust-wasm-bindgen-shared-0.2.104
                                                    rust-wayland-backend-0.3.11
                                                    rust-wayland-client-0.31.11
                                                    rust-wayland-protocols-0.32.9
                                                    rust-wayland-scanner-0.31.7
                                                    rust-wayland-sys-0.31.7
                                                    rust-web-sys-0.3.81
                                                    rust-web-time-1.1.0
                                                    rust-webpki-roots-0.25.4
                                                    rust-webpki-roots-1.0.4
                                                    rust-whatadistro-0.1.0
                                                    rust-which-4.4.2
                                                    rust-winapi-0.3.9
                                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                                    rust-winapi-util-0.1.11
                                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                                    rust-wincompatlib-0.7.7
                                                    rust-windows-0.61.3
                                                    rust-windows-collections-0.2.0
                                                    rust-windows-core-0.61.2
                                                    rust-windows-future-0.2.1
                                                    rust-windows-implement-0.60.0
                                                    rust-windows-interface-0.59.1
                                                    rust-windows-link-0.1.3
                                                    rust-windows-link-0.2.0
                                                    rust-windows-numerics-0.2.0
                                                    rust-windows-result-0.3.4
                                                    rust-windows-strings-0.4.2
                                                    rust-windows-sys-0.52.0
                                                    rust-windows-sys-0.59.0
                                                    rust-windows-sys-0.60.2
                                                    rust-windows-sys-0.61.0
                                                    rust-windows-targets-0.52.6
                                                    rust-windows-targets-0.53.3
                                                    rust-windows-threading-0.1.0
                                                    rust-windows-aarch64-gnullvm-0.52.6
                                                    rust-windows-aarch64-gnullvm-0.53.0
                                                    rust-windows-aarch64-msvc-0.52.6
                                                    rust-windows-aarch64-msvc-0.53.0
                                                    rust-windows-i686-gnu-0.52.6
                                                    rust-windows-i686-gnu-0.53.0
                                                    rust-windows-i686-gnullvm-0.52.6
                                                    rust-windows-i686-gnullvm-0.53.0
                                                    rust-windows-i686-msvc-0.52.6
                                                    rust-windows-i686-msvc-0.53.0
                                                    rust-windows-x86-64-gnu-0.52.6
                                                    rust-windows-x86-64-gnu-0.53.0
                                                    rust-windows-x86-64-gnullvm-0.52.6
                                                    rust-windows-x86-64-gnullvm-0.53.0
                                                    rust-windows-x86-64-msvc-0.52.6
                                                    rust-windows-x86-64-msvc-0.53.0
                                                    rust-winnow-0.7.13
                                                    rust-wit-bindgen-0.46.0
                                                    rust-writeable-0.6.1
                                                    rust-xattr-1.6.1
                                                    rust-xz-0.1.0
                                                    rust-xz2-0.1.7
                                                    rust-yoke-0.8.0
                                                    rust-yoke-derive-0.8.0
                                                    rust-zbus-5.11.0
                                                    rust-zbus-macros-5.11.0
                                                    rust-zbus-names-4.2.0
                                                    rust-zerocopy-0.8.27
                                                    rust-zerocopy-derive-0.8.27
                                                    rust-zerofrom-0.1.6
                                                    rust-zerofrom-derive-0.1.6
                                                    rust-zeroize-1.8.1
                                                    rust-zeroize-derive-1.4.2
                                                    rust-zerotrie-0.2.2
                                                    rust-zerovec-0.11.4
                                                    rust-zerovec-derive-0.11.1
                                                    rust-zip-3.0.0
                                                    rust-zlib-rs-0.5.2
                                                    rust-zopfli-0.8.2
                                                    rust-zstd-0.13.3
                                                    rust-zstd-safe-7.2.4
                                                    rust-zstd-sys-2.0.16+zstd.1.5.7
                                                    rust-zvariant-5.7.0
                                                    rust-zvariant-derive-5.7.0
                                                    rust-zvariant-utils-3.2.1)))
