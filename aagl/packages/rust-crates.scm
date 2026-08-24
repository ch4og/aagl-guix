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
  #:use-module ((aagl packages rust-sources) #:prefix package:)
  #:export (lookup-cargo-inputs))

;;;
;;; This file is managed by ‘guix import’.  Do NOT add definitions manually.
;;;

;;;
;;; Rust libraries fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)

(define rust-aho-corasick-1.1.5
  (crate-source "aho-corasick" "1.1.5"
                "1fhjkp2nbs7gg4y1b68hpc8028rpax8aiscfh9b60q78m4pn90n9"))

(define rust-anime-game-core-1.36.3.044a1e8
  package:rust-anime-game-core-1.36.3.044a1e8)

(define rust-anime-game-core-1.38.8.de96f35
  package:rust-anime-game-core-1.38.8.de96f35)

(define rust-anime-game-core-1.38.10.ba60faf
  package:rust-anime-game-core-1.38.10.ba60faf)

(define rust-anime-game-core-1.39.1.aa8c5ce
  package:rust-anime-game-core-1.39.1.aa8c5ce)

(define rust-anime-game-core-1.39.3.f1f0a23
  package:rust-anime-game-core-1.39.3.f1f0a23)

(define rust-anime-launcher-sdk-1.32.0.87c4206
  package:rust-anime-launcher-sdk-1.32.0.87c4206)

(define rust-anime-launcher-sdk-1.35.10.c0991af
  package:rust-anime-launcher-sdk-1.35.10.c0991af)

(define rust-anime-launcher-sdk-1.35.12.4b9cb6e
  package:rust-anime-launcher-sdk-1.35.12.4b9cb6e)

(define rust-anime-launcher-sdk-1.36.4.bf66ccb
  package:rust-anime-launcher-sdk-1.36.4.bf66ccb)

(define rust-anime-launcher-sdk-1.36.7.c682598
  package:rust-anime-launcher-sdk-1.36.7.c682598)

(define rust-anyhow-1.0.104
  (crate-source "anyhow" "1.0.104"
                "0w34jjcm02p5g9kvsjr1dvpw0zs2fi7igi6nr414fkm5gz85w2ik"))

(define rust-arrayvec-0.7.8
  (crate-source "arrayvec" "0.7.8"
                "0mmd8lrijbvg1qp4c5zis5dq41a3mjv2rb6bxkyj9kwaw2k6gyyk"))

(define rust-async-trait-0.1.92
  (crate-source "async-trait" "0.1.92"
                "0rqn5iga1hlv2lm8xzav1zhar46jb4dvx89i6kfv93kb53maxxl2"))

(define rust-atty-0.2.14
  (crate-source "atty" "0.2.14"
                "1s7yslcs6a28c5vz7jwj63lkfgyx8mx99fdirlhi9lbhhzhrpcyr"))

(define rust-autocfg-1.5.1
  (crate-source "autocfg" "1.5.1"
                "0lqasy5i30flcgih1b50kvsk6z32g09r1q4ql7q81pj6228jy0zj"))

(define rust-bitflags-2.13.1
  (crate-source "bitflags" "2.13.1"
                "1nl76mpykmwmb8rq1l5vw1azdh1wvxdrnsk4sy3rdrzx01nvg25m"))

(define rust-blake3-1.8.6
  (crate-source "blake3" "1.8.6"
                "0xrap6fg0z1ip0v0xb2f906gjmh7662w7brvqvjfj8214nnppbkn"))

(define rust-blake3-1.8.7
  (crate-source "blake3" "1.8.7"
                "1bi7m95sggkkcmlyiykflbb37v84cgzkm283r1yrfxhzq57lb7kd"))

(define rust-bstr-1.13.1
  (crate-source "bstr" "1.13.1"
                "0pxyrnp8nb2iwcbadzird7xr2awrbjzi2jwqx47f4i22q531pcvb"))

(define rust-bumpalo-3.20.3
  (crate-source "bumpalo" "3.20.3"
                "0jc6va3nwcqikm7chnpdv1s87my3gs2j7g1sc7g3k91brg3arxbj"))

(define rust-bytes-1.12.1
  (crate-source "bytes" "1.12.1"
                "017z19dpg4f942h051m7bpnzcgng042hhcpd7bmg7bjjqd42lrgw"))

(define rust-cc-1.4.2
  (crate-source "cc" "1.4.2"
                "0zi7dyd4jaflww22jd3701869jrv4p47f9xlslw7h60pk4a2w9jx"))

(define rust-cc-1.4.4
  (crate-source "cc" "1.4.4"
                "0wq26vvhzv5ci9gx3cfiw320skvaysf9i701wp668lks6ps39m8a"))

(define rust-cfg-aliases-0.2.2
  (crate-source "cfg_aliases" "0.2.2"
                "09rm3dv28gbsal7w6q76lg2nfyn8wp789ska9b8vr1w750xfhygh"))

(define rust-cfg-expr-0.20.8
  (crate-source "cfg-expr" "0.20.8"
                "0z4r6l4936g1c1s27ryvjdy5pjij6sfvs3myk3hji9dgpi13asgv"))

(define rust-cfg-expr-0.20.9
  (crate-source "cfg-expr" "0.20.9"
                "05z97ah29viw66l3c46y391nfsri9dxpwr166k36dxxmfj2cwkpy"))

(define rust-chacha20-0.10.1
  (crate-source "chacha20" "0.10.1"
                "108aajbvs3rwl4d0pdvq3p8ydy4pwh0rxy2z265ynwkflrmla96m"))

(define rust-clap-4.6.6
  (crate-source "clap" "4.6.6"
                "1jmx5z8d6jbvxdz6dybh599s4rd7ns6sl90p2rrdga09yh3pwg27"))

(define rust-clap-builder-4.6.6
  (crate-source "clap_builder" "4.6.6"
                "12cqg25zpjc3k82cpqa2v9h7s3vk1vydpgnwl8lfg6lfm2jzwj3v"))

(define rust-clap-derive-4.6.4
  (crate-source "clap_derive" "4.6.4"
                "0qd0v7pa2arwxjjinmjim6xrjy61bc28m1yryhc7zjjssswx44nh"))

(define rust-clap-lex-1.1.0
  (crate-source "clap_lex" "1.1.0"
                "1ycqkpygnlqnndghhcxjb44lzl0nmgsia64x9581030yifxs7m68"))

(define rust-console-0.16.4
  (crate-source "console" "0.16.4"
                "0z5sik90c39ywvkdkdc5bqrkbj441ycmvf21mn7yizpnlijz9rag"))

(define rust-crc32fast-1.5.1
  (crate-source "crc32fast" "1.5.1"
                "0l75bfakpwr86wz45gm38lylrpgbssr529fmm6m445qy2rqwi644"))

(define rust-crossbeam-channel-0.5.16
  (crate-source "crossbeam-channel" "0.5.16"
                "17k72dh5qqkh0xvqzr27wny7gl1l7fgzlvh2xxx71jmfgz1n6lyq"))

(define rust-crossbeam-deque-0.8.7
  (crate-source "crossbeam-deque" "0.8.7"
                "1sqcxia1mmz2fw8ba1v72jjrvbkvg7c6sz9l3sl07sv1gggf10ai"))

(define rust-crossbeam-epoch-0.9.20
  (crate-source "crossbeam-epoch" "0.9.20"
                "0gzg0v8in20iajikalg5i5qgpp0m26r426f0fs8nwk953w218s9d"))

(define rust-crossbeam-utils-0.8.22
  (crate-source "crossbeam-utils" "0.8.22"
                "05vwf7pmjq8c8f3fp5qqdm0z3cnk4p62wi8spf0jms5yjnh3v031"))

(define rust-dialoguer-0.12.0
  (crate-source "dialoguer" "0.12.0"
                "15mdq2cp838yiq9fs1jkhvskixvlqz5p8f8dipkn88xz06sh9w95"))

(define rust-displaydoc-0.2.7
  (crate-source "displaydoc" "0.2.7"
                "1a42mwpgpwcqq2qqgkcc630wvsc2p2dkmgacjnclginwfz9js8y6"))

(define rust-either-1.17.0
  (crate-source "either" "1.17.0"
                "07dagpwcfdzpkb1n7fxkx0q3nv80rnf81v7gwlz9ljx22mn8yply"))

(define rust-either-1.18.0
  (crate-source "either" "1.18.0"
                "0d7dx31sf8rakcgp63070ngb2vkjynrni866pnx879pawndgnai5"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-enum-ordinalize-4.4.2
  (crate-source "enum-ordinalize" "4.4.2"
                "0xvnafjy52n9fmf0zmw6cm2p286ifm84fiq6rw89an09kda03pc9"))

(define rust-enum-ordinalize-derive-4.4.2
  (crate-source "enum-ordinalize-derive" "4.4.2"
                "05f87ad4f78lyr1yxj12l31pjf1xjv647xghsa5qiqjcbb8n6n56"))

(define rust-event-listener-5.4.2
  (crate-source "event-listener" "5.4.2"
                "1lk9sv7r07l58jk263s18896l55mx9jv0g1rm4hj2mpi3paas8ss"))

(define rust-fastrand-2.5.0
  (crate-source "fastrand" "2.5.0"
                "08q2r30y62winysimnlpbvw9kiwn0rmdlidqlmzd6z90mv764z6s"))

(define rust-filetime-0.2.29
  (crate-source "filetime" "0.2.29"
                "0napyyfccb26r7fyh9hg7ixrh4vph9h7y7k4iv1j19phqwrpla2w"))

(define rust-find-msvc-tools-0.1.10
  (crate-source "find-msvc-tools" "0.1.10"
                "1pp1612g5k6im9732g16j6a87czhb35xcyzlrpq2mkgdwrrkbdr6"))

(define rust-find-msvc-tools-0.1.11
  (crate-source "find-msvc-tools" "0.1.11"
                "145qpfb9r4ml2klr8v4byvrkikp61qyiks9n69b8z0vbscbb0pfl"))

(define rust-fluent-template-macros-0.13.3
  (crate-source "fluent-template-macros" "0.13.3"
                "0v3cd26jnb3mjbmznnixwxiwk28k0ag8xzzdd9b7pnbgzfrm103l"))

(define rust-fluent-templates-0.13.3
  (crate-source "fluent-templates" "0.13.3"
                "0xllpndnwkd5s7v2nj2j1y5gcdnplka5zk7rmrll8h0zl13489jn"))

(define rust-futures-0.3.34
  (crate-source "futures" "0.3.34"
                "18yhwmbdalhz2z9i1vm10hy2v0cfm82dkgcb6vr2msxazfix4ccs"))

(define rust-futures-channel-0.3.34
  (crate-source "futures-channel" "0.3.34"
                "1i4kwcanpaphn1ax62ci3nx176kglxqx0gnhzqpqdr1rkpbf7ydi"))

(define rust-futures-core-0.3.34
  (crate-source "futures-core" "0.3.34"
                "0pjgv4fx0np6hrs5sz5a2phabwv0z70yr51v03injbi44bjrkmlj"))

(define rust-futures-executor-0.3.34
  (crate-source "futures-executor" "0.3.34"
                "0cjl3y7jgg60wwb96ikxj23r6q91ylvx8v675yychv1w3b7lf6q3"))

(define rust-futures-io-0.3.34
  (crate-source "futures-io" "0.3.34"
                "1v9z6wj92ra18kpv0xig21hgpzrvcwmcr8fszyzh64yyay0zmh2k"))

(define rust-futures-macro-0.3.34
  (crate-source "futures-macro" "0.3.34"
                "0i0czvcvsqq4hrccibq2f23004si5z34zjwdxfmqhlrmm15nbfcz"))

(define rust-futures-sink-0.3.34
  (crate-source "futures-sink" "0.3.34"
                "07cfvrgc3vxk6sw5g8a8dnrm1mzg6d5mwy08ywa1sgyhyxml4i0r"))

(define rust-futures-task-0.3.34
  (crate-source "futures-task" "0.3.34"
                "1zfilqs8nwlfqz4prk7ihvpp5avvzins87ibzlxzq5fhs7ipshfd"))

(define rust-futures-util-0.3.34
  (crate-source "futures-util" "0.3.34"
                "1g3r9ghzq7c2fh34lis43i72xavk9p84npgfwgb5vfpqcwjajl0d"))

(define rust-generator-0.8.9
  (crate-source "generator" "0.8.9"
                "1bhk2m8alf9nfmmq2y2whyriigppgjnzrchq7yix3sl4wnq59f5k"))

(define rust-getrandom-0.4.3
  (crate-source "getrandom" "0.4.3"
                "16b0202fkdwz3p2cyll82dv24ljbn0wiyy829v4lwbkbflyqh3ih"))

(define rust-globset-0.4.20
  (crate-source "globset" "0.4.20"
                "1249r63326pzaz6z1c04mmdp6dqg6z3kni47jyylans622a4mhq7"))

(define rust-h2-0.4.15
  (crate-source "h2" "0.4.15"
                "0mgilh1g8gydcchqi6acs5l6j0gwg5jwpa64sj4b3ncb9v497c3c"))

(define rust-h2-0.4.19
  (crate-source "h2" "0.4.19"
                "05mw60jmsq97vjgj607nxjkx8dl6rxv6jj9i4r2z92056id5x3pg"))

(define rust-hashbrown-0.17.1
  (crate-source "hashbrown" "0.17.1"
                "0jmqz7i4yl6cm7rbn0i2ffkfrmwi6xkmzkaldr2v8bcsx2v0jngd"))

(define rust-hermit-abi-0.1.19
  (crate-source "hermit-abi" "0.1.19"
                "0cxcm8093nf5fyn114w8vxbrbcyvv91d4015rdnlgfll7cs6gd32"))

(define rust-http-1.5.0
  (crate-source "http" "1.5.0"
                "1q4wpz5hb4cf37g3jrdyffrpa6ngidmd9wrfph92fddzprl3b3ci"))

(define rust-http-body-1.1.0
  (crate-source "http-body" "1.1.0"
                "0b5wj0rdj8p03k20q8x0jy249amg2db919fnmh7zcrgf2clqyana"))

(define rust-http-body-util-0.1.5
  (crate-source "http-body-util" "0.1.5"
                "07773iilap808wjp6vywlq15zkgwnswqzrv270zxvg2z9biry5i3"))

(define rust-hyper-1.11.0
  (crate-source "hyper" "1.11.0"
                "0wha96biivgpj0fpf80a2aar5dfbff1lk62i9x9i2bl53wl5686j"))

(define rust-icu-collections-2.3.0
  (crate-source "icu_collections" "2.3.0"
                "04x59h6vdq0cnpippim1nr471ivlsnnn470sj1d5v864h48d4s7s"))

(define rust-icu-locale-core-2.3.0
  (crate-source "icu_locale_core" "2.3.0"
                "1sqdj16wwl7h9y6r7j394av4kpdb7zryz9h169ffwbm9imc2hvnm"))

(define rust-icu-normalizer-2.3.0
  (crate-source "icu_normalizer" "2.3.0"
                "0vv43ixk2wmbxrx7kl33cwkhx1wdyb1q3pa18qkyshan4dgwzy8j"))

(define rust-icu-normalizer-data-2.3.0
  (crate-source "icu_normalizer_data" "2.3.0"
                "1811h0ppb7lwq1q2492p5x6lcmlwmhbmkf69fhyvzcz0scgdlqqm"))

(define rust-icu-properties-2.3.0
  (crate-source "icu_properties" "2.3.0"
                "0j51hi8qgf0l6a7qzvnwsc61598w2fpnsklicld6ci9immva4z3y"))

(define rust-icu-properties-data-2.3.0
  (crate-source "icu_properties_data" "2.3.0"
                "1akw1gp5rcaiz377xzsnkx8f92qax4kh3lfn9y4rcjj6q4wg1475"))

(define rust-icu-provider-2.3.0
  (crate-source "icu_provider" "2.3.0"
                "0a343jlrb7jlb20xv1airslzv63559wf38jihrx81bba39kyv9wj"))

(define rust-icu-provider-2.3.1
  (crate-source "icu_provider" "2.3.1"
                "0wrydhwprwgyka3r3sw6276syjnlw6fpqr2zsm2srvxv7afvnyyj"))

(define rust-ignore-0.4.33
  (crate-source "ignore" "0.4.33"
                "0bs0d0qy8n6n2r4s01s234vr33ngssb1sm0rgnmcb7bjxlrridh0"))

(define rust-indicatif-0.18.6
  (crate-source "indicatif" "0.18.6"
                "037vk2cr5b0iwri5023wjyww7d4gqpjcf8f0g6x1mv5lsrn80cwl"))

(define rust-interprocess-2.4.3
  (crate-source "interprocess" "2.4.3"
                "0wlmfr3nsxvwfcc5468695p3k0dgd4j4q5640incq5557d1y33br"))

(define rust-ipnet-2.12.1
  (crate-source "ipnet" "2.12.1"
                "0y6xssyvfy85k90pnnanxv9phayxalhp8bacy61rw4vkmhznqxba"))

(define rust-jobserver-0.1.35
  (crate-source "jobserver" "0.1.35"
                "1crwgbb0wjph42ni4hqryjxlv4vlr0hyk81g76id9fpa56ysq00w"))

(define rust-js-sys-0.3.104
  (crate-source "js-sys" "0.3.104"
                "0fjsgady7wbv7bbyy6c8qhrd93bnx11qbl83l1g7bb9a4601030f"))

(define rust-libc-0.2.189
  (crate-source "libc" "0.2.189"
                "1whjfs375vlng2q6yrbzs73cvp5lm3w1n2gfqajb2vgf7zg3xbry"))

(define rust-litemap-0.8.3
  (crate-source "litemap" "0.8.3"
                "1bpgpj87560hmckh3875fbahpmfxbk4g8pzns84h3ykf3nfx3na7"))

(define rust-log-0.4.33
  (crate-source "log" "0.4.33"
                "1bd9dmk22pxgnf0h0slba6rz99zb0a0b2mdhpk8p92bp26ycbvhc"))

(define rust-log-0.4.34
  (crate-source "log" "0.4.34"
                "1ihkzn0m33ab79fcl4mkb04n5iwqzbxzyw7l7hazqkffaqzbvy7r"))

(define rust-loom-0.7.2
  (crate-source "loom" "0.7.2"
                "1jpszf9qxv8ydpsm2h9vcyvxvyxcfkhmmfbylzd4gfbc0k40v7j1"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-memchr-2.8.3
  (crate-source "memchr" "2.8.3"
                "161xa63ipfanf8v3nb82xd5hqgydv55nzw59wyngqbz6alfaz2yg"))

(define rust-mio-1.2.2
  (crate-source "mio" "1.2.2"
                "09y4b7gc42ymgssshh8sz6gs3y5r8bbigqaw2c4snh6fy5qmrmih"))

(define rust-num-conv-0.2.2
  (crate-source "num-conv" "0.2.2"
                "0hg4f9bwmy7cwpxdkm165dmkfc8jhkkayci234jsmi5ssb33j5sj"))

(define rust-open-5.4.1
  (crate-source "open" "5.4.1"
                "0204wdb57cvc12fbjxfdv9mx1wf066p4kng3qy468j4wgs9yzkzr"))

(define rust-open-5.4.2
  (crate-source "open" "5.4.2"
                "0mfxj1p2s8fz1vb9zfn22mw76sqp9kq1ac0krqvza7mwci3bxqxd"))

(define rust-pkg-config-0.3.34
  (crate-source "pkg-config" "0.3.34"
                "0j05h08nzg0q8rf6lzw7nry0b7kn7x97vc9n4hwrl52fqzxn9d7n"))

(define rust-portable-atomic-1.15.0
  (crate-source "portable-atomic" "1.15.0"
                "11csag858ndk5w4yz17h91vy53ynh67r2903gwwdn2cnilzbdj05"))

(define rust-potential-utf-0.1.6
  (crate-source "potential_utf" "0.1.6"
                "0qbndl2fpphq7mph41m11vaixs05xrh1s451wxlgap4fdnybjgnq"))

(define rust-proc-macro2-1.0.107
  (crate-source "proc-macro2" "1.0.107"
                "1nb6ly8kp65f724kj73ippc7lvydss24sm2vagk6qpklpg4pwplq"))

(define rust-prost-0.14.4
  (crate-source "prost" "0.14.4"
                "1qas5v5rap45f43v3ja0jngxrrafrkcwl0iw5a3ld1pz2rscd2jj"))

(define rust-prost-build-0.14.4
  (crate-source "prost-build" "0.14.4"
                "0hmh8nqxa0r6h7rv5rhl2yw0pmszq1h4hza09mmbni7z05w09nh3"))

(define rust-prost-derive-0.14.4
  (crate-source "prost-derive" "0.14.4"
                "1pqa77d7da5pf6ba3kjj7510m5cynz6902ax01ckvr0pfrgv4w5m"))

(define rust-prost-types-0.14.4
  (crate-source "prost-types" "0.14.4"
                "02ivjvc4cwl5bfgjs3l00hwlrk74z8zlg1xcgx60bww8fvf6fjgr"))

(define rust-quick-xml-0.41.0
  (crate-source "quick-xml" "0.41.0"
                "1h9y8zry34r3mxfd5vqfj50vvvzvri4kzbx5d657jkqjalg4aq76"))

(define rust-quinn-0.11.11
  (crate-source "quinn" "0.11.11"
                "1a60yxn03zr07ll7zianby2mrs18w4frgm1c6y4x9fxn6zj426hc"))

(define rust-quinn-proto-0.11.16
  (crate-source "quinn-proto" "0.11.16"
                "0q75f2wkhc7iw8n0q63jb3zm7206b7774l44r1ixzfb2a80zqjrg"))

(define rust-quinn-proto-0.11.17
  (crate-source "quinn-proto" "0.11.17"
                "10xskd9f8qynnkmg8vw3058fynd6jhi22a3f2c4kgs9vah894x84"))

(define rust-quinn-udp-0.5.15
  (crate-source "quinn-udp" "0.5.15"
                "15063ji7443y4z8i4pdxlid2vn0kkxjc51d6c6dfiaysavwk789m"))

(define rust-quote-1.0.47
  (crate-source "quote" "1.0.47"
                "00ch0yyzvv6s671ik0kcsbw8nigdaj2g3fr61kcahwx48aqlvgqz"))

(define rust-rand-0.10.2
  (crate-source "rand" "0.10.2"
                "105yqkdzqbgggd3r1yjm9jg0zvibfdsmxylvxxkmblwc0lxgmxf7"))

(define rust-rand-0.9.5
  (crate-source "rand" "0.9.5"
                "0hbvllk8g28mqjld6hqmckk69w296qpzg95whm3didsyg46ivvxr"))

(define rust-rand-pcg-0.10.2
  (crate-source "rand_pcg" "0.10.2"
                "0sp817pvwb3d2nxb1ww1y0f8x3kc4w198j2iqvs742hwgq9z986a"))

(define rust-regex-1.13.1
  (crate-source "regex" "1.13.1"
                "1391a0a4100ik8cp7l577p3ip3haqq03rd9c5vdr7vcfdixj687h"))

(define rust-regex-automata-0.4.18
  (crate-source "regex-automata" "0.4.18"
                "1cml0rm0ssqfkibh9nh3gy4b6hbsbicj1rihpwf2a4v4nawm71dd"))

(define rust-regex-syntax-0.8.11
  (crate-source "regex-syntax" "0.8.11"
                "1m25h5q2wp976fb9gc3dsc9l99svcvd5cri8lncb51c46ydgzxnn"))

(define rust-rustc-demangle-0.1.28
  (crate-source "rustc-demangle" "0.1.28"
                "1sr083jamg89zcxmchia1pdn584smsy2r32kk9q30a5vm3zmcjxp"))

(define rust-rustc-hash-2.1.3
  (crate-source "rustc-hash" "2.1.3"
                "0bbla578m87qmf3yr55q49l97gxn7z0ha1dwqlnvwwc58ad7y7kb"))

(define rust-rustls-0.23.43
  (crate-source "rustls" "0.23.43"
                "01nsagj78r88pifaz55ln1rw31py5n00h7bnw58h3g1aw1n3i0q2"))

(define rust-rustls-pki-types-1.15.1
  (crate-source "rustls-pki-types" "1.15.1"
                "15hakk4pcvr5278cazgw9qf2r7gdg09rg5pivbyd3dbyih12aj9g"))

(define rust-rustls-webpki-0.103.14
  (crate-source "rustls-webpki" "0.103.14"
                "0njk28gvbqrsfg1b5r35y4f80n37kcjylj72fpc0k0g60n3529q5"))

(define rust-rustls-webpki-0.103.15
  (crate-source "rustls-webpki" "0.103.15"
                "1hhanq3lz384v4nccacnjfwsyy99n3yc6m6iw8kljz8yicfwzhzk"))

(define rust-rustversion-1.0.23
  (crate-source "rustversion" "1.0.23"
                "07z2a843fs80fawwflj9jwn49k9b0bd0dhhbvy0ar69vaxd72m6g"))

(define rust-self-cell-1.3.0
  (crate-source "self_cell" "1.3.0"
                "04x883z7awzkmn5lqb67n51xynrj2pa9339jgq4j1qa94yh2rd1a"))

(define rust-serde-1.0.229
  (crate-source "serde" "1.0.229"
                "1fp04fq4a79bpm61xz1zy0pbz4kpc7d771zii1k3inmszq55jj21"))

(define rust-serde-core-1.0.229
  (crate-source "serde_core" "1.0.229"
                "0j1ajiha76h3nmd976il9li6975k121xa7jb39ws8n0yqp4s5p37"))

(define rust-serde-derive-1.0.229
  (crate-source "serde_derive" "1.0.229"
                "0j4k63i7h1bikxwz2c89ig0hrwbnl9mz1czn85xx99x5cc9dg9g7"))

(define rust-serde-json-1.0.151
  (crate-source "serde_json" "1.0.151"
                "051zww7lvpw147vvwss1ng6w587qyrkzg75fvj08q2dfrmgbahf8"))

(define rust-serde-repr-0.1.21
  (crate-source "serde_repr" "0.1.21"
                "01l987ghc17h1y9cf9xbzmcs77575mbrjf4ca2h70g15vqlicfwd"))

(define rust-sha1-0.10.7
  (crate-source "sha1" "0.10.7"
                "1f632d529qzz95yrprr632w1fxqkrv6b6jksjc11vnzl049lay59"))

(define rust-shell-words-1.1.1
  (crate-source "shell-words" "1.1.1"
                "0xzd5p53xl0ndnk63r0by52rhdrh6pd37szfxszkg73zb6ffcvyw"))

(define rust-shlex-2.0.1
  (crate-source "shlex" "2.0.1"
                "1fjsll1cd7d2bcpdij9kd6w62rpbc7qqzvydvs021vsmr1cxvypq"))

(define rust-simd-adler32-0.3.10
  (crate-source "simd-adler32" "0.3.10"
                "1sny4y2qa5mwyxx5x59ln2p02vsdh92004njlslnx98imjc9489s"))

(define rust-smallvec-1.15.2
  (crate-source "smallvec" "1.15.2"
                "143wzbqf6vgapdp2z4qpl0yvlqcn17s8cnk8m28rqly808zsdmlf"))

(define rust-socket2-0.6.5
  (crate-source "socket2" "0.6.5"
                "1m7diygswpvlpvrxd6ap169nxgax014jr8220nqlr3bzyb3y5lf3"))

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

(define rust-aho-corasick-1.1.4
  (crate-source "aho-corasick" "1.1.4"
                "00a32wb2h07im3skkikc495jvncf62jl6s96vwc7bhi70h9imlyx"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-anstream-1.0.0
  (crate-source "anstream" "1.0.0"
                "13d2bj0xfg012s4rmq44zc8zgy1q8k9yp7yhvfnarscnmwpj2jl2"))

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-anstyle-parse-1.0.0
  (crate-source "anstyle-parse" "1.0.0"
                "03hkv2690s0crssbnmfkr76kw1k7ah2i6s5amdy9yca2n8w7zkjj"))

(define rust-anstyle-query-1.1.5
  (crate-source "anstyle-query" "1.1.5"
                "1p6shfpnbghs6jsa0vnqd8bb8gd7pjd0jr7w0j8jikakzmr8zi20"))

(define rust-anstyle-wincon-3.0.11
  (crate-source "anstyle-wincon" "3.0.11"
                "0zblannm70sk3xny337mz7c6d8q8i24vhbqi42ld8v7q1wjnl7i9"))

(define rust-anyhow-1.0.102
  (crate-source "anyhow" "1.0.102"
                "0b447dra1v12z474c6z4jmicdmc5yxz5bakympdnij44ckw2s83z"))

(define rust-arbitrary-1.4.2
  (crate-source "arbitrary" "1.4.2"
                "1wcbi4x7i3lzcrkjda4810nqv03lpmvfhb0a85xrq1mbqjikdl63"))

(define rust-arrayref-0.3.9
  (crate-source "arrayref" "0.3.9"
                "1jzyp0nvp10dmahaq9a2rnxqdd5wxgbvp8xaibps3zai8c9fi8kn"))

(define rust-arrayvec-0.7.6
  (crate-source "arrayvec" "0.7.6"
                "0l1fz4ccgv6pm609rif37sl5nv5k6lbzi7kkppgzqzh1vwix20kw"))

(define rust-ashpd-0.11.1
  (crate-source "ashpd" "0.11.1"
                "0fcw7n3ssbc8qvkaqbjrqs3sgjk4k0sfn50301ax2ky7anbzgwyj"))

(define rust-async-broadcast-0.7.2
  (crate-source "async-broadcast" "0.7.2"
                "0ckmqcwyqwbl2cijk1y4r0vy60i89gqc86ijrxzz5f2m4yjqfnj3"))

(define rust-async-recursion-1.1.1
  (crate-source "async-recursion" "1.1.1"
                "04ac4zh8qz2xjc79lmfi4jlqj5f92xjvfaqvbzwkizyqd4pl4hrv"))

(define rust-atomic-waker-1.1.2
  (crate-source "atomic-waker" "1.1.2"
                "1h5av1lw56m0jf0fd3bchxq8a30xv0b4wv8s4zkp4s0i7mfvs18m"))

(define rust-backtrace-0.3.76
  (crate-source "backtrace" "0.3.76"
                "1mibx75x4jf6wz7qjifynld3hpw3vq6sy3d3c9y5s88sg59ihlxv"))

(define rust-base64-0.21.7
  (crate-source "base64" "0.21.7"
                "0rw52yvsk75kar9wgqfwgb414kvil1gn7mqkrhn9zf1537mpsacx"))

(define rust-base64-0.22.1
  (crate-source "base64" "0.22.1"
                "1imqzgh7bxcikp5vx3shqvw9j09g9ly0xr0jma0q66i52r7jbcvj"))

(define rust-bitflags-2.11.1
  (crate-source "bitflags" "2.11.1"
                "1cvqijg3rvwgis20a66vfdxannjsxfy5fgjqkaq3l13gyfcj4lf4"))

(define rust-blake3-1.8.5
  (crate-source "blake3" "1.8.5"
                "1khz6wq61fnr0gl1kmy4bxadc7gbcv4gbq05z4jdjhr8wqs3ra0a"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-bumpalo-3.20.2
  (crate-source "bumpalo" "3.20.2"
                "1jrgxlff76k9glam0akhwpil2fr1w32gbjdf5hpipc7ld2c7h82x"))

(define rust-byteorder-1.5.0
  (crate-source "byteorder" "1.5.0"
                "0jzncxyf404mwqdbspihyzpkndfgda450l0893pz5xj685cg5l0z"))

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

(define rust-cc-1.2.61
  (crate-source "cc" "1.2.61"
                "0vawvnrrsmi8dygavq3wx085cmlp10sp3fhld5842rlqkqsr0vfi"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-cipher-0.4.4
  (crate-source "cipher" "0.4.4"
                "1b9x9agg67xq5nq879z66ni4l08m6m3hqcshk37d4is4ysd3ngvp"))

(define rust-colorchoice-1.0.5
  (crate-source "colorchoice" "1.0.5"
                "0w75k89hw39p0mnnhlrwr23q50rza1yjki44qvh2mgrnj065a1qx"))

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
  (crate-source "core-foundation-sys" "0.8.7"
                "12w8j73lazxmr1z0h98hf3z623kl8ms7g07jch7n4p8f9nwlhdkp"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-cpufeatures-0.3.0
  (crate-source "cpufeatures" "0.3.0"
                "00fjhygsqmh4kbxxlb99mcsbspxcai6hjydv4c46pwb67wwl2alb"))

(define rust-crc-3.4.0
  (crate-source "crc" "3.4.0"
                "03dsq5qsv86m35ikg84l80d00wnkjm8q4pjxgac0vaqjrnhs5f2y"))

(define rust-crc-catalog-2.5.0
  (crate-source "crc-catalog" "2.5.0"
                "0lq8dl60g457za8la86pak6i7ydxanm2lrpkqh5kyjkbz7m9hxi1"))

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

(define rust-deflate64-0.1.12
  (crate-source "deflate64" "0.1.12"
                "1qj6a4y5b7jb6h8rgjj4b8lgi6b310hpn43fl6zn176z2rjr4sxc"))

(define rust-deranged-0.5.8
  (crate-source "deranged" "0.5.8"
                "0711df3w16vx80k55ivkwzwswziinj4dz05xci3rvmn15g615n3w"))

(define rust-derive-arbitrary-1.4.2
  (crate-source "derive_arbitrary" "1.4.2"
                "0annkmfwfavd978vwwrxvrpykjfdnc3w6q1ln3j7kyfg5pc7nmhy"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dispatch2-0.3.1
  (crate-source "dispatch2" "0.3.1"
                "0f5xmnbzpaz1g80m27kd804p75nswh0ikb6wvqh4ba3x9rz3c3hy"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-dlib-0.5.3
  (crate-source "dlib" "0.5.3"
                "0jpr4smrwrv8xj70mz4ixnbc6ljm82f12z2mz1hv89056y3wv3mb"))

(define rust-dns-lookup-2.1.1
  (crate-source "dns-lookup" "2.1.1"
                "1kwkfxqmyk1dflrhy4yhhz5y9j3b68kcx26gkpy7alpynyj9fmfg"))

(define rust-doctest-file-1.1.1
  (crate-source "doctest-file" "1.1.1"
                "0nfkscv8gf3ixhradrmwm3f2p6sc0ab0psah7c8976ha9zkh9ny2"))

(define rust-downcast-rs-1.2.1
  (crate-source "downcast-rs" "1.2.1"
                "1lmrq383d1yszp7mg5i7i56b17x2lnn3kb91jwsq0zykvg2jbcvm"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-endi-1.1.1
  (crate-source "endi" "1.1.1"
                "16a0076dx41vgrzzimm9clcym77h732czqjiajanmzvd1i1y5dv6"))

(define rust-enum-ordinalize-4.3.2
  (crate-source "enum-ordinalize" "4.3.2"
                "1w0012dqq5y3xikpswix190jvjl097pjrzpi515jr3qzpfkr242a"))

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

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-event-listener-strategy-0.5.4
  (crate-source "event-listener-strategy" "0.5.4"
                "14rv18av8s7n8yixg38bxp5vg2qs394rl1w052by5npzmbgz7scb"))

(define rust-fastrand-2.4.1
  (crate-source "fastrand" "2.4.1"
                "1mnqxxnxvd69ma9mczabpbbsgwlhd6l78yv3vd681453a9s247wz"))

(define rust-field-offset-0.3.6
  (crate-source "field-offset" "0.3.6"
                "0zq5sssaa2ckmcmxxbly8qgz3sxpb8g1lwv90sdh1z74qif2gqiq"))

(define rust-filetime-0.2.27
  (crate-source "filetime" "0.2.27"
                "1nspbkm1d1km7xfljcbl565swqxrihqyin8bqppig2gf3qal927r"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-fixedbitset-0.5.7
  (crate-source "fixedbitset" "0.5.7"
                "16fd3v9d2cms2vddf9xhlm56sz4j0zgrk3d2h6v1l7hx760lwrqx"))

(define rust-flate2-1.1.9
  (crate-source "flate2" "1.1.9"
                "0g2pb7cxnzcbzrj8bw4v6gpqqp21aycmf6d84rzb6j748qkvlgw4"))

(define rust-fluent-bundle-0.15.3
  (crate-source "fluent-bundle" "0.15.3"
                "14zl0cjn361is69pb1zry4k2zzh5nzsfv0iz05wccl00x0ga5q3z"))

(define rust-fluent-bundle-0.16.0
  (crate-source "fluent-bundle" "0.16.0"
                "1x1v8bmym6x9pl87f82lbzwlc84kdn0lgcwi73ki2mwgj6w3q801"))

(define rust-fluent-langneg-0.13.1
  (crate-source "fluent-langneg" "0.13.1"
                "1c78jl8lpwg5hdg589qbn3m9ls6mzqxnyrvi5llfibhb8mcvxsvy"))

(define rust-fluent-syntax-0.11.1
  (crate-source "fluent-syntax" "0.11.1"
                "0gd3cdvsx9ymbb8hijcsc9wyf8h1pbcbpsafg4ldba56ji30qlra"))

(define rust-fluent-syntax-0.12.0
  (crate-source "fluent-syntax" "0.12.0"
                "1661sp6kl268n445x7jjhnbkgiaa1xcpyryq0i6iiz9zqn3x5w2l"))

(define rust-fluent-template-macros-0.11.0
  (crate-source "fluent-template-macros" "0.11.0"
                "02pj22rv9j0bv9wcwcfsivywzznc56m05pr58anp6kwaarp1fz80"))

(define rust-fluent-templates-0.11.0
  (crate-source "fluent-templates" "0.11.0"
                "1b5p5fvgdz2bgkjcc0cl105kdrc42hw6mh8kx9ii2mf8n9hjzwkl"))

(define rust-flume-0.11.1
  (crate-source "flume" "0.11.1"
                "15ch0slxa8sqsi6c73a0ky6vdnh48q8cxjf7rksa3243m394s3ns"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fragile-2.1.0
  (crate-source "fragile" "2.1.0"
                "1ygw6asivdkyiy7a427hq17bvspr31pzsas1ia0nxf2bl55qcy48"))

(define rust-fs-extra-1.3.0
  (crate-source "fs_extra" "1.3.0"
                "075i25z70j2mz9r7i9p9r521y8xdj81q7skslyb7zhqnnw33fw22"))

(define rust-fs2-0.4.3
  (crate-source "fs2" "0.4.3"
                "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))

(define rust-futures-channel-0.3.32
  (crate-source "futures-channel" "0.3.32"
                "07fcyzrmbmh7fh4ainilf1s7gnwvnk07phdq77jkb9fpa2ffifq7"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-io-0.3.32
  (crate-source "futures-io" "0.3.32"
                "063pf5m6vfmyxj74447x8kx9q8zj6m9daamj4hvf49yrg9fs7jyf"))

(define rust-futures-lite-2.6.1
  (crate-source "futures-lite" "2.6.1"
                "1ba4dg26sc168vf60b1a23dv1d8rcf3v3ykz2psb7q70kxh113pp"))

(define rust-futures-sink-0.3.32
  (crate-source "futures-sink" "0.3.32"
                "14q8ml7hn5a6gyy9ri236j28kh0svqmrk4gcg0wh26rkazhm95y3"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-gdk-pixbuf-0.20.10
  (crate-source "gdk-pixbuf" "0.20.10"
                "0371cfhxldrn2pf8zdjyx2b3xkhbfm96k988spp4nkq89j4l5lig"))

(define rust-gdk-pixbuf-sys-0.20.10
  (crate-source "gdk-pixbuf-sys" "0.20.10"
                "15hb2f5mmyg5amaha6lx6spaygw2b7ga4hwmgqhvv269h2sz6d2v"))

(define rust-gdk4-0.9.6
  (crate-source "gdk4" "0.9.6"
                "0q1dld01fgj7qxj644by0fc242mcn36w3bagn4z1mkdfq7cwjl28"))

(define rust-gdk4-sys-0.9.6
  (crate-source "gdk4-sys" "0.9.6"
                "0fj722lp86fpa1b1i3s2anavdmcpybd0b47mkhknzd72k1bvjvkg"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-getrandom-0.2.17
  (crate-source "getrandom" "0.2.17"
                "1l2ac6jfj9xhpjjgmcx6s1x89bbnw9x6j9258yy6xjkzpq0bqapz"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.2
  (crate-source "getrandom" "0.4.2"
                "0mb5833hf9pvn9dhvxjgfg5dx0m77g8wavvjdpvpnkp9fil1xr8d"))

(define rust-gimli-0.32.3
  (crate-source "gimli" "0.32.3"
                "1iqk5xznimn5bfa8jy4h7pa1dv3c624hzgd2dkz8mpgkiswvjag6"))

(define rust-gio-0.20.12
  (crate-source "gio" "0.20.12"
                "0cdq5116cwdgs0xkdp1v146yhcqilxlpgvkncc7xbf5nwxvf49wf"))

(define rust-gio-sys-0.20.10
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
  (crate-source "glib-sys" "0.20.10"
                "05f29ky5dnvy8vp5rdld5f8r2lgr5w7dxqr7p27km016s4g9xdwa"))

(define rust-gobject-sys-0.20.10
  (crate-source "gobject-sys" "0.20.10"
                "1niyqv22b2c38ks33i4isas4v83d3w7jx3xzzly9x63kpfacm6pc"))

(define rust-graphene-rs-0.20.10
  (crate-source "graphene-rs" "0.20.10"
                "16six67j0j57ynv7frxiwnsf7dslhyy67ppirad1q98lgnnxz1kb"))

(define rust-graphene-sys-0.20.10
  (crate-source "graphene-sys" "0.20.10"
                "1sk1736b4vay2hj9qz56c0pvqa3v0mkdch3yg7hiapidpa2kln6z"))

(define rust-gsk4-0.9.6
  (crate-source "gsk4" "0.9.6"
                "0mgqq5m6cm4q7ajjgw92z13z2ikpvh6zx2gwzdjrz30wjcpygxb1"))

(define rust-gsk4-sys-0.9.6
  (crate-source "gsk4-sys" "0.9.6"
                "1p1n4jhhxyvj7hb0cqhzvazrck0qw81sz36ydfj8avzsapg5jl3m"))

(define rust-gtk4-0.9.7
  (crate-source "gtk4" "0.9.7"
                "1mi6lcwm25jz7lznrb9glaabgyk40hnvkg4fzaxlf762080xsx7j"))

(define rust-gtk4-macros-0.9.5
  (crate-source "gtk4-macros" "0.9.5"
                "169rqfxfczivcpz7019slsrpkx8crqjka43ymxmikp838xn7il8f"))

(define rust-gtk4-sys-0.9.6
  (crate-source "gtk4-sys" "0.9.6"
                "1mh3xjkjb99y97z234cvyar08vcr7zblg1nrw48c6xsdwl0kpq21"))

(define rust-h2-0.4.13
  (crate-source "h2" "0.4.13"
                "0m6w5gg0n0m1m5915bxrv8n4rlazhx5icknkslz719jhh4xdli1g"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.17.0
  (crate-source "hashbrown" "0.17.0"
                "0l8gvcz80lvinb7x22h53cqbi2y1fm603y2jhhh9qwygvkb7sijg"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hex-0.4.3
  (crate-source "hex" "0.4.3"
                "0w1a4davm1lgzpamwnba907aysmlrnygbqmfis2mqjx5m552a93z"))

(define rust-hmac-0.12.1
  (crate-source "hmac" "0.12.1"
                "0pmbr069sfg76z7wsssfk5ddcqd9ncp79fyz6zcm6yn115yc6jbc"))

(define rust-home-0.5.12
  (crate-source "home" "0.5.12"
                "13bjyzgx6q9srnfvl43dvmhn93qc8mh5w7cylk2g13sj3i3pyqnc"))

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

(define rust-human-panic-2.0.8
  (crate-source "human-panic" "2.0.8"
                "0gfcr5s89gl08z5ppiq8ivj25f2h1qyhp5jkjw56wg3p9465v0g2"))

(define rust-hyper-1.9.0
  (crate-source "hyper" "1.9.0"
                "1jmwbwqcaficskg76kq402gbymbnh2z4v99xwq3l5aa6n8bg16b2"))

(define rust-hyper-rustls-0.27.9
  (crate-source "hyper-rustls" "0.27.9"
                "03vfnsm873wsp1dk0q85nxvk7w6syp8c2m5bcdjcyfgg4786ijik"))

(define rust-hyper-util-0.1.20
  (crate-source "hyper-util" "0.1.20"
                "186zdc58hmm663csmjvrzgkr6jdh93sfmi3q2pxi57gcaqjpqm4n"))

(define rust-icu-collections-2.2.0
  (crate-source "icu_collections" "2.2.0"
                "070r7xd0pynm0hnc1v2jzlbxka6wf50f81wybf9xg0y82v6x3119"))

(define rust-icu-locale-core-2.2.0
  (crate-source "icu_locale_core" "2.2.0"
                "0a9cmin5w1x3bg941dlmgszn33qgq428k7qiqn5did72ndi9n8cj"))

(define rust-icu-normalizer-2.2.0
  (crate-source "icu_normalizer" "2.2.0"
                "1d7krxr0xpc4x9635k1100a24nh0nrc59n65j6yk6gbfkplmwvn5"))

(define rust-icu-normalizer-data-2.2.0
  (crate-source "icu_normalizer_data" "2.2.0"
                "0f5d5d5fhhr9937m2z6z38fzh6agf14z24kwlr6lyczafypf0fys"))

(define rust-icu-properties-2.2.0
  (crate-source "icu_properties" "2.2.0"
                "1pkh3s837808cbwxvfagwc28cvwrz2d9h5rl02jwrhm51ryvdqxy"))

(define rust-icu-properties-data-2.2.0
  (crate-source "icu_properties_data" "2.2.0"
                "052awny0qwkbcbpd5jg2cd7vl5ry26pq4hz1nfsgf10c3qhbnawf"))

(define rust-icu-provider-2.2.0
  (crate-source "icu_provider" "2.2.0"
                "08dl8pxbwr8zsz4c5vphqb7xw0hykkznwi4rw7bk6pwb3krlr70k"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-ident-case-1.0.1
  (crate-source "ident_case" "1.0.1"
                "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.2
  (crate-source "idna_adapter" "1.2.2"
                "0557p76l8hj35r9zn1yv7c6x1c0qbrsffmg80n0yy8361ly3fs6b"))

(define rust-indexmap-2.14.0
  (crate-source "indexmap" "2.14.0"
                "1na9z6f0d5pkjr1lgsni470v98gv2r7c41j8w48skr089x2yjrnl"))

(define rust-inout-0.1.4
  (crate-source "inout" "0.1.4"
                "008xfl1jn9rxsq19phnhbimccf4p64880jmnpg59wqi07kk117w7"))

(define rust-intl-memoizer-0.5.3
  (crate-source "intl-memoizer" "0.5.3"
                "0gqn5wwhzacvj0z25r5r3l2pajg9c8i1ivh7g8g8dszm8pis439i"))

(define rust-intl-pluralrules-7.0.2
  (crate-source "intl_pluralrules" "7.0.2"
                "0wprd3h6h8nfj62d8xk71h178q7zfn3srxm787w4sawsqavsg3h7"))

(define rust-ipnet-2.12.0
  (crate-source "ipnet" "2.12.0"
                "1qpq2y0asyv0jppw7zww9y96fpnpinwap8a0phhqqgyy3znnz3yr"))

(define rust-iri-string-0.7.12
  (crate-source "iri-string" "0.7.12"
                "082fpx6c5ghvmqpwxaf2b268m47z2ic3prajqbmi1s1qpfj5kri5"))

(define rust-is-docker-0.2.0
  (crate-source "is-docker" "0.2.0"
                "1cyibrv6817cqcpf391m327ss40xlbik8wxcv5h9pj9byhksx2wj"))

(define rust-is-terminal-polyfill-1.70.2
  (crate-source "is_terminal_polyfill" "1.70.2"
                "15anlc47sbz0jfs9q8fhwf0h3vs2w4imc030shdnq54sny5i7jx6"))

(define rust-is-wsl-0.4.0
  (crate-source "is-wsl" "0.4.0"
                "19bs5pq221d4bknnwiqqkqrnsx2in0fsk8fylxm1747iim4hjdhp"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itoa-1.0.18
  (crate-source "itoa" "1.0.18"
                "10jnd1vpfkb8kj38rlkn2a6k02afvj3qmw054dfpzagrpl6achlg"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-js-sys-0.3.95
  (crate-source "js-sys" "0.3.95"
                "1jhj3kgxxgwm0cpdjiz7i2qapqr7ya9qswadmr63dhwx3lnyjr19"))

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
  (crate-source "libadwaita-sys" "0.7.2"
                "1nqjr514hhdc4aldlsc4y3vkpnkq9q73g2jl7ypqnmf2b209i036"))

(define rust-libc-0.2.186
  (crate-source "libc" "0.2.186"
                "0rnyhzjyqq9x56skkllbjzzzwym3r61lq3l4hqj64v71gw0r3av8"))

(define rust-libloading-0.8.9
  (crate-source "libloading" "0.8.9"
                "0mfwxwjwi2cf0plxcd685yxzavlslz7xirss3b9cbrzyk4hv1i6p"))

(define rust-libredox-0.1.16
  (crate-source "libredox" "0.1.16"
                "0v54zvgknag9310wcjykgv86pgq02qr3mzgkdg4r6m1k7ns3nbz0"))

(define rust-linux-raw-sys-0.12.1
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-linux-raw-sys-0.4.15
  (crate-source "linux-raw-sys" "0.4.15"
                "1aq7r2g7786hyxhv40spzf2nhag5xbw2axxc1k8z5k1dsgdm4v6j"))

(define rust-litemap-0.8.2
  (crate-source "litemap" "0.8.2"
                "1w7628bc7wwcxc4n4s5kw0610xk06710nh2hn5kwwk2wa91z9nlj"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

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
  (crate-source "lzma-sys" "0.1.20"
                "09sxp20waxyglgn3cjz8qjkspb3ryz2fwx4rigkwvrk46ymh9njz"))

(define rust-md-5-0.10.6
  (crate-source "md-5" "0.10.6"
                "1kvq5rnpm4fzwmyv5nmnxygdhhb2369888a06gdc9pxyrzh7x7nq"))

(define rust-md5-asm-0.5.2
  (crate-source "md5-asm" "0.5.2"
                "1pz217kwlvrw4bj4hil5acyp3l7g37vwf25psdc210bxzkkqx6yi"))

(define rust-memchr-2.8.0
  (crate-source "memchr" "2.8.0"
                "0y9zzxcqxvdqg6wyag7vc3h0blhdn7hkq164bxyx2vph8zs5ijpq"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-minreq-2.14.1
  (crate-source "minreq" "2.14.1"
                "0g8mw9fncqngxrwkvyxfcid6h1lhknfpwd6aj4bddxyhv8152085"))

(define rust-mio-1.2.0
  (crate-source "mio" "1.2.0"
                "1hanrh4fwsfkdqdaqfidz48zz1wdix23zwn3r2x78am0garfbdsh"))

(define rust-multimap-0.10.1
  (crate-source "multimap" "0.10.1"
                "1150lf0hjfjj4ksb8s3y0hl7a2nqzqlbh0is7vdym2iyjfrfr1qx"))

(define rust-nanorand-0.7.0
  (crate-source "nanorand" "0.7.0"
                "1hr60b8zlfy7mxjcwx2wfmhpkx7vfr3v9x12shmv1c10b0y32lba"))

(define rust-ntapi-0.4.3
  (crate-source "ntapi" "0.4.3"
                "1bl0d73avwla7laa4pkqvzvifjbs0avg65w01zxjydgx3likbcy3"))

(define rust-nu-ansi-term-0.50.3
  (crate-source "nu-ansi-term" "0.50.3"
                "1ra088d885lbd21q1bxgpqdlk1zlndblmarn948jz2a40xsbjmvr"))

(define rust-num-conv-0.2.1
  (crate-source "num-conv" "0.2.1"
                "0rqrr29brafaa2za352pbmhkk556n7f8z9rrkgmjp1idvdl3fry6"))

(define rust-objc2-0.6.4
  (crate-source "objc2" "0.6.4"
                "17x8qpl512frscfqbmgjr20kg3y4r0xdqxphja17dz5f0znsh4is"))

(define rust-objc2-app-kit-0.3.2
  (crate-source "objc2-app-kit" "0.3.2"
                "132ijwni8lsi8phq7wnmialkxp46zx998fns3zq5np0ya1mr77nl"))

(define rust-objc2-core-foundation-0.3.2
  (crate-source "objc2-core-foundation" "0.3.2"
                "0dnmg7606n4zifyjw4ff554xvjmi256cs8fpgpdmr91gckc0s61a"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-objc2-foundation-0.3.2
  (crate-source "objc2-foundation" "0.3.2"
                "0wijkxzzvw2xkzssds3fj8279cbykz2rz9agxf6qh7y2agpsvq73"))

(define rust-objc2-io-kit-0.3.2
  (crate-source "objc2-io-kit" "0.3.2"
                "05dvfcf97w39daaj5qsbfc399lw9hbx3s4h9nwgxrmlpjnizpyik"))

(define rust-object-0.37.3
  (crate-source "object" "0.37.3"
                "1zikiy9xhk6lfx1dn2gn2pxbnfpmlkn0byd7ib1n720x0cgj0xpz"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-once-cell-polyfill-1.70.2
  (crate-source "once_cell_polyfill" "1.70.2"
                "1zmla628f0sk3fhjdjqzgxhalr2xrfna958s632z65bjsfv8ljrq"))

(define rust-openssl-probe-0.1.6
  (crate-source "openssl-probe" "0.1.6"
                "0bl52x55laalqb707k009h8kfawliwp992rlsvkzy49n47p2fpnh"))

(define rust-ordered-stream-0.2.0
  (crate-source "ordered-stream" "0.2.0"
                "0l0xxp697q7wiix1gnfn66xsss7fdhfivl2k7bvpjs4i3lgb18ls"))

(define rust-paimon-0.1.0.2f80905
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://dawn.wine/lily/paimon")
          (commit "2f80905d9e711e81bf5c80216a0760c30271da33")))
    (file-name (git-file-name "rust-paimon" "0.1.0.2f80905"))
    (sha256 (base32 "16fvspbx5hvhf0qifvy1irlbimdcrj4cv4iywnnjrsil5yz36y5z"))))

(define rust-pango-0.20.12
  (crate-source "pango" "0.20.12"
                "0p5bj7k8sd2pgm7v907i9bip53ys46hahprs0jbr6rfzyq8v6xk5"))

(define rust-pango-sys-0.20.10
  (crate-source "pango-sys" "0.20.10"
                "1yj3n87whqx6gw3vip08zbckqxfg7l5jqc2wamaf76y07xkhjs8q"))

(define rust-parking-2.2.1
  (crate-source "parking" "2.2.1"
                "1fnfgmzkfpjd69v4j9x737b1k8pnn054bvzcn5dm3pkgq595d3gk"))

(define rust-pbkdf2-0.12.2
  (crate-source "pbkdf2" "0.12.2"
                "1wms79jh4flpy1zi8xdp4h8ccxv4d85adc6zjagknvppc5vnmvgq"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-petgraph-0.8.3
  (crate-source "petgraph" "0.8.3"
                "0mblnaqbx1y20h5y7pz6y11hk9jjk6k87lsmn7jxaq3hm67ba0c7"))

(define rust-pin-project-lite-0.2.17
  (crate-source "pin-project-lite" "0.2.17"
                "1kfmwvs271si96zay4mm8887v5khw0c27jc9srw1a75ykvgj54x8"))

(define rust-pkg-config-0.3.33
  (crate-source "pkg-config" "0.3.33"
                "17jnqmcbxsnwhg9gjf0nh6dj5k0x3hgwi3mb9krjnmfa9v435w8r"))

(define rust-plain-0.2.3
  (crate-source "plain" "0.2.3"
                "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))

(define rust-pollster-0.4.0
  (crate-source "pollster" "0.4.0"
                "1qqcn0h2bvmgm9rlhfrdk7lfaiw1ad86g9500bhx1rj1s0c9yfig"))

(define rust-potential-utf-0.1.5
  (crate-source "potential_utf" "0.1.5"
                "0r0518fr32xbkgzqap509s3r60cr0iancsg9j1jgf37cyz7b20q1"))

(define rust-powerfmt-0.2.0
  (crate-source "powerfmt" "0.2.0"
                "14ckj2xdpkhv3h6l5sdmb9f1d57z8hbfpdldjc2vl5givq2y77j3"))

(define rust-ppv-lite86-0.2.21
  (crate-source "ppv-lite86" "0.2.21"
                "1abxx6qz5qnd43br1dd9b2savpihzjza8gb4fbzdql1gxp2f7sl5"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.5.0
  (crate-source "proc-macro-crate" "3.5.0"
                "0kv1g1d1zjwxlgcaba2qlshzyy32j03xic8rskqlcr5mnblsfyz6"))

(define rust-proc-macro-hack-0.5.20+deprecated
  (crate-source "proc-macro-hack" "0.5.20+deprecated"
                "0s402hmcs3k9nd6rlp07zkr1lz7yimkmcwcbgnly2zr44wamwdyw"))

(define rust-proc-macro2-1.0.106
  (crate-source "proc-macro2" "1.0.106"
                "0d09nczyaj67x4ihqr5p7gxbkz38gxhk4asc0k8q23g9n85hzl4g"))

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

(define rust-quinn-0.11.9
  (crate-source "quinn" "0.11.9"
                "086gzj666dr3slmlynkvxlndy28hahgl361d6bf93hk3i6ahmqmr"))

(define rust-quinn-proto-0.11.14
  (crate-source "quinn-proto" "0.11.14"
                "1660jkxhzi1pnywzs13ifczwrlv6ds9qds111vsnxjciqpz44js3"))

(define rust-quinn-udp-0.5.14
  (crate-source "quinn-udp" "0.5.14"
                "1gacawr17a2zkyri0r3m0lc9spzmxbq1by3ilyb8v2mdvjhcdpmd"))

(define rust-quote-1.0.45
  (crate-source "quote" "1.0.45"
                "095rb5rg7pbnwdp6v8w5jw93wndwyijgci1b5lw8j1h5cscn3wj1"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-rand-0.9.4
  (crate-source "rand" "0.9.4"
                "1sknbxgs6nfg0nxdd7689lwbyr2i4vaswchrv4b34z8vpc3azia4"))

(define rust-rand-chacha-0.9.0
  (crate-source "rand_chacha" "0.9.0"
                "1jr5ygix7r60pz0s1cv3ms1f6pd1i9pcdmnxzzhjc3zn3mgjn0nk"))

(define rust-rand-core-0.10.1
  (crate-source "rand_core" "0.10.1"
                "0s9wiacxrr100icl7i41308gcj85nlcclrc5jx1jd6p10dhigf33"))

(define rust-rand-core-0.9.5
  (crate-source "rand_core" "0.9.5"
                "0g6qc5r3f0hdmz9b11nripyp9qqrzb0xqk9piip8w8qlvqkcibvn"))

(define rust-raw-window-handle-0.6.2
  (crate-source "raw-window-handle" "0.6.2"
                "0ff5c648hncwx7hm2a8fqgqlbvbl4xawb6v3xxv9wkpjyrr5arr0"))

(define rust-recvmsg-1.0.0
  (crate-source "recvmsg" "1.0.0"
                "0xa173gbg1cx8q7wyzi6c4kmcsz5rka68r4jb6kg14icskax9vfk"))

(define rust-redox-syscall-0.7.4
  (crate-source "redox_syscall" "0.7.4"
                "0fk4infcfn2hvshrwgf7r48rf9mr1zxy1a28d7xn798x7ffasl7l"))

(define rust-regex-1.12.3
  (crate-source "regex" "1.12.3"
                "0xp2q0x7ybmpa5zlgaz00p8zswcirj9h8nry3rxxsdwi9fhm81z1"))

(define rust-regex-automata-0.4.14
  (crate-source "regex-automata" "0.4.14"
                "13xf7hhn4qmgfh784llcp2kzrvljd13lb2b1ca0mwnf15w9d87bf"))

(define rust-regex-syntax-0.8.10
  (crate-source "regex-syntax" "0.8.10"
                "02jx311ka0daxxc7v45ikzhcl3iydjbbb0mdrpc1xgg8v7c7v2fw"))

(define rust-relm4-0.9.1
  (crate-source "relm4" "0.9.1"
                "1las0fjxmiblq9bv0q0hiqszk3swghwfr0sc80dfmkx8q59pb0rh"))

(define rust-relm4-css-0.9.0
  (crate-source "relm4-css" "0.9.0"
                "1y9prwy1jrwznajayqz4y3dhyqkn9cy322xnhz3ds76zax2r4fqx"))

(define rust-relm4-macros-0.9.1
  (crate-source "relm4-macros" "0.9.1"
                "1pnx5spw2akrjqrgcgayig992bx94jypk9hc21yqa6j4ams5m2as"))

(define rust-reqwest-0.12.28
  (crate-source "reqwest" "0.12.28"
                "0iqidijghgqbzl3bjg5hb4zmigwa4r612bgi0yiq0c90b6jkrpgd"))

(define rust-rfd-0.15.4
  (crate-source "rfd" "0.15.4"
                "1vgbfi32843dysqdq3fv604qzqll8al82z9dqwsldyngwrhywazg"))

(define rust-ring-0.17.14
  (crate-source "ring" "0.17.14"
                "1dw32gv19ccq4hsx3ribhpdzri1vnrlcfqb2vj41xn4l49n9ws54"))

(define rust-rustc-hash-1.1.0
  (crate-source "rustc-hash" "1.1.0"
                "1qkc5khrmv5pqi5l5ca9p5nl5hs742cagrndhbrlk3dhlrx3zm08"))

(define rust-rustc-hash-2.1.2
  (crate-source "rustc-hash" "2.1.2"
                "1gjdc5bw9982cj176jvgz9rrqf9xvr1q1ddpzywf5qhs7yzhlc4l"))

(define rust-rustc-version-0.4.1
  (crate-source "rustc_version" "0.4.1"
                "14lvdsmr5si5qbqzrajgb6vfn69k0sfygrvfvr2mps26xwi3mjyg"))

(define rust-rustix-0.38.44
  (crate-source "rustix" "0.38.44"
                "0m61v0h15lf5rrnbjhcb9306bgqrhskrqv7i1n0939dsw8dbrdgx"))

(define rust-rustix-1.1.4
  (crate-source "rustix" "1.1.4"
                "14511f9yjqh0ix07xjrjpllah3325774gfwi9zpq72sip5jlbzmn"))

(define rust-rustls-0.21.12
  (crate-source "rustls" "0.21.12"
                "0gjdg2a9r81sdwkyw3n5yfbkrr6p9gyk3xr2kcsr3cs83x6s2miz"))

(define rust-rustls-0.23.39
  (crate-source "rustls" "0.23.39"
                "03p6fkdwbdpp93dfidc4nzgmalwp3gxnv0rk421a5k3pn2612b3w"))

(define rust-rustls-native-certs-0.6.3
  (crate-source "rustls-native-certs" "0.6.3"
                "007zind70rd5rfsrkdcfm8vn09j8sg02phg9334kark6rdscxam9"))

(define rust-rustls-pemfile-1.0.4
  (crate-source "rustls-pemfile" "1.0.4"
                "1324n5bcns0rnw6vywr5agff3rwfvzphi7rmbyzwnv6glkhclx0w"))

(define rust-rustls-pki-types-1.14.1
  (crate-source "rustls-pki-types" "1.14.1"
                "1a9pr54y0f3qr97bxpd3ahjldq0gqdld0h799xbnwdzbwxx1k9rh"))

(define rust-rustls-webpki-0.101.7
  (crate-source "rustls-webpki" "0.101.7"
                "0rapfhpkqp75552i8r0y7f4vq7csb4k7gjjans0df73sxv8paqlb"))

(define rust-rustls-webpki-0.103.13
  (crate-source "rustls-webpki" "0.103.13"
                "0vkm7z9pnxz5qz66p2kmyy2pwx0g4jnsbqk5xzfhs4czcjl2ki31"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.23
  (crate-source "ryu" "1.0.23"
                "0zs70sg00l2fb9jwrf6cbkdyscjs53anrvai2hf7npyyfi5blx4p"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schannel-0.1.29
  (crate-source "schannel" "0.1.29"
                "0ffrzz5vf2s3gnzvphgb5gg8fqifvryl07qcf7q3x1scj3jbghci"))

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

(define rust-security-framework-sys-2.17.0
  (crate-source "security-framework-sys" "2.17.0"
                "1qr0w0y9iwvmv3hwg653q1igngnc5b74xcf0679cbv23z0fnkqkc"))

(define rust-self-cell-0.10.3
  (crate-source "self_cell" "0.10.3"
                "0pci3zh23b7dg6jmlxbn8k4plb7hcg5jprd1qiz0rp04p1ilskp1"))

(define rust-semver-1.0.28
  (crate-source "semver" "1.0.28"
                "1kaimrpy876bcgi8bfj0qqfxk77zm9iz2zhn1hp9hj685z854y4a"))

(define rust-serde-1.0.228
  (crate-source "serde" "1.0.228"
                "17mf4hhjxv5m90g42wmlbc61hdhlm6j9hwfkpcnd72rpgzm993ls"))

(define rust-serde-core-1.0.228
  (crate-source "serde_core" "1.0.228"
                "1bb7id2xwx8izq50098s5j2sqrrvk31jbbrjqygyan6ask3qbls1"))

(define rust-serde-derive-1.0.228
  (crate-source "serde_derive" "1.0.228"
                "0y8xm7fvmr2kjcd029g9fijpndh8csv5m20g4bd76w8qschg4h6m"))

(define rust-serde-json-1.0.149
  (crate-source "serde_json" "1.0.149"
                "11jdx4vilzrjjd1dpgy67x5lgzr0laplz30dhv75lnf5ffa07z43"))

(define rust-serde-spanned-1.1.1
  (crate-source "serde_spanned" "1.1.1"
                "09jzk7i6wihn3d8i3wi4j4n98ghi93c3b8m8k64nxq0ijn3vaqk6"))

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

(define rust-signal-hook-registry-1.4.8
  (crate-source "signal-hook-registry" "1.4.8"
                "06vc7pmnki6lmxar3z31gkyg9cw7py5x9g7px70gy2hil75nkny4"))

(define rust-simd-adler32-0.3.9
  (crate-source "simd-adler32" "0.3.9"
                "0532ysdwcvzyp2bwpk8qz0hijplcdwpssr5gy5r7qwqqy5z5qgbh"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.15.1
  (crate-source "smallvec" "1.15.1"
                "00xxdxxpgyq5vjnpljvkmy99xij5rxgh913ii1v16kzynnivgcb7"))

(define rust-socket2-0.6.3
  (crate-source "socket2" "0.6.3"
                "0gkjjcyn69hqhhlh5kl8byk5m0d7hyrp2aqwzbs3d33q208nwxis"))

(define rust-sophon-lib-0.1.6.89f4a70
  package:rust-sophon-lib-0.1.6.89f4a70)

(define rust-sophon-lib-0.1.8.58d223a
  package:rust-sophon-lib-0.1.8.58d223a)

(define rust-sophon-lib-0.1.9.f422286
  package:rust-sophon-lib-0.1.9.f422286)

(define rust-spin-0.9.9
  (crate-source "spin" "0.9.9"
                "03psal0vh1xdxp7agphw09p7kf50v3bj1zshijq1s5bkdd7jcqrp"))

(define rust-stable-deref-trait-1.2.1
  (crate-source "stable_deref_trait" "1.2.1"
                "15h5h73ppqyhdhx6ywxfj88azmrpml9gl6zp3pwy2malqa6vxqkc"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-subtle-2.6.1
  (crate-source "subtle" "2.6.1"
                "14ijxaymghbl1p0wql9cib5zlwiina7kall6w7g89csprkgbvhhk"))

(define rust-syn-2.0.117
  (crate-source "syn" "2.0.117"
                "16cv7c0wbn8amxc54n4w15kxlx5ypdmla8s0gxr2l7bv7s0bhrg6"))

(define rust-syn-2.0.119
  (crate-source "syn" "2.0.119"
                "15vjy620l91a3q4n4f4gzhnflmdr6pnm38v2m6cpk86i8av32a47"))

(define rust-syn-3.0.3
  (crate-source "syn" "3.0.3"
                "18srnql3cd39j9q6hf1az02p67rlr1rf6njx9zx4vxj9i3jvmsak"))

(define rust-syn-3.0.4
  (crate-source "syn" "3.0.4"
                "17v4ac61x0hvj1879ywqzlwhyzg7n9lr9zniwrsif3b1ykfmq9z6"))

(define rust-sync-wrapper-1.0.2
  (crate-source "sync_wrapper" "1.0.2"
                "0qvjyasd6w18mjg5xlaq5jgy84jsjfsvmnn12c13gypxbv75dwhb"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-sysinfo-0.35.2
  (crate-source "sysinfo" "0.35.2"
                "0vj9j8m10j56a41552f1qckv3isnjqs3rsvsgyjj9czj9wzglgrw"))

(define rust-sysinfo-0.38.4
  (crate-source "sysinfo" "0.38.4"
                "0bx5wjp16cyckr9c0fxzrfcx54g4aa15f1k47kmqsl7yicpnmawj"))

(define rust-system-deps-7.0.8
  (crate-source "system-deps" "7.0.8"
                "1rwnfw9dm6ck65a7lfjfpn2c91gwj88brz2i09z3fdbknvz3asir"))

(define rust-tar-0.4.45
  (crate-source "tar" "0.4.45"
                "0wq90hif25348zrvmk88q01g8aj8v8pla7f1vxgsf7x2frj2ls92"))

(define rust-tar-0.4.46
  (crate-source "tar" "0.4.46"
                "0h68bc0y1nma3h2ypj28vxc84msjydlrj8rviqwphg00lvcj2qiz"))

(define rust-target-lexicon-0.13.5
  (crate-source "target-lexicon" "0.13.5"
                "1jm6lmf9hsn7ri2d6v9gg6fy24lylhskh6pbxh71f82wdxd97dmd"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.18
  (crate-source "thiserror" "2.0.18"
                "1i7vcmw9900bvsmay7mww04ahahab7wmr8s925xc083rpjybb222"))

(define rust-thiserror-2.0.20
  (crate-source "thiserror" "2.0.20"
                "0kxs6p295jffxhzaxpxv1dwaaf5iqlm6sx8h0djp6ancbxgj71pc"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.18
  (crate-source "thiserror-impl" "2.0.18"
                "1mf1vrbbimj1g6dvhdgzjmn6q09yflz2b92zs1j9n3k7cxzyxi7b"))

(define rust-thiserror-impl-2.0.20
  (crate-source "thiserror-impl" "2.0.20"
                "1bwjc94gi0xn5jz26h1a8bjj1wdkvvr6jifamyc4mp9n28zcs15w"))

(define rust-thread-local-1.1.10
  (crate-source "thread_local" "1.1.10"
                "0w20g2pfdcp8pz3gds0bzksv6mxk802szca8qlr3701jdm69rn8s"))

(define rust-time-0.3.47
  (crate-source "time" "0.3.47"
                "0b7g9ly2iabrlgizliz6v5x23yq5d6bpp0mqz6407z1s526d8fvl"))

(define rust-time-0.3.55
  (crate-source "time" "0.3.55"
                "0d6iyws47z50zlksf5m3cflxvjrcgfhjglhn112gmpahxjappf6d"))

(define rust-time-core-0.1.8
  (crate-source "time-core" "0.1.8"
                "1jidl426mw48i7hjj4hs9vxgd9lwqq4vyalm4q8d7y4iwz7y353n"))

(define rust-time-core-0.1.9
  (crate-source "time-core" "0.1.9"
                "028ix0ax7ixp1h1k5zsqwgw85w6y1q32irslma7ci6ddd5kr074y"))

(define rust-tinystr-0.8.3
  (crate-source "tinystr" "0.8.3"
                "0vfr8x285w6zsqhna0a9jyhylwiafb2kc8pj2qaqaahw48236cn8"))

(define rust-tinystr-0.8.4
  (crate-source "tinystr" "0.8.4"
                "0hzncw8rgk4syla79qscfml46jm7ll1zdp7kdacc42cj8n8prqmi"))

(define rust-tinyvec-1.11.0
  (crate-source "tinyvec" "1.11.0"
                "1wvycrghzmaysnw34kzwnf0mfx6r75045s24r214wnnjadqfcq9y"))

(define rust-tinyvec-1.12.0
  (crate-source "tinyvec" "1.12.0"
                "0zxaid976y60f4722vjhfnwcbydmzpwva7p03aqzl15gl3dblkmv"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-tokio-1.52.1
  (crate-source "tokio" "1.52.1"
                "1imw1dkkv38p66i33m5hsyk3d6prsbyrayjvqhndjvz89ybywzdn"))

(define rust-tokio-1.53.1
  (crate-source "tokio" "1.53.1"
                "1v8b3b45pkpbibls75yniqbvx5dlks2708141ljni5mnf6lawb10"))

(define rust-tokio-rustls-0.26.4
  (crate-source "tokio-rustls" "0.26.4"
                "0qggwknz9w4bbsv1z158hlnpkm97j3w8v31586jipn99byaala8p"))

(define rust-tokio-util-0.7.18
  (crate-source "tokio-util" "0.7.18"
                "1600rd47pylwn7cap1k7s5nvdaa9j7w8kqigzp1qy7mh0p4cxscs"))

(define rust-tokio-util-0.7.19
  (crate-source "tokio-util" "0.7.19"
                "0licqrhrawysjrsr0qw3cgzkkjph7090hlcqcm45aazmkg81aj29"))

(define rust-toml-1.1.4+spec-1.1.0
  (crate-source "toml" "1.1.4+spec-1.1.0"
                "1xanf3v10j8hdjz37mkhg80w92cw25kxwndhcp4w5pxw9czydb1s"))

(define rust-toml-datetime-1.1.1+spec-1.1.0
  (crate-source "toml_datetime" "1.1.1+spec-1.1.0"
                "1mws2mkkf46l7inn77azhm0vdwxngv9vsbhbl0ah33p2c9gzcr9i"))

(define rust-toml-edit-0.25.13+spec-1.1.0
  (crate-source "toml_edit" "0.25.13+spec-1.1.0"
                "16xgmjdnxssdpj7rjyimsk4fqbv29g8zl7zhdbc6dxrf9mz3cxb9"))

(define rust-toml-parser-1.1.3+spec-1.1.0
  (crate-source "toml_parser" "1.1.3+spec-1.1.0"
                "0mjdvihdkmjd4ykh574xgii71hpxw7ns7h4n4bisqpxrz4faqf0x"))

(define rust-toml-writer-1.1.2+spec-1.1.0
  (crate-source "toml_writer" "1.1.2+spec-1.1.0"
                "1lk6pqf9mac3v1x6282n6a66qx5b18c8f4a23bsd0nk658x3amkx"))

(define rust-tower-0.5.3
  (crate-source "tower" "0.5.3"
                "1m5i3a2z1sgs8nnz1hgfq2nr4clpdmizlp1d9qsg358ma5iyzrgb"))

(define rust-tower-http-0.6.11
  (crate-source "tower-http" "0.6.11"
                "0h08wjgs3hwnq11iwwzlmnabn1h4cl0fzd48svaccvqffkiggz2c"))

(define rust-tower-http-0.6.8
  (crate-source "tower-http" "0.6.8"
                "1y514jwzbyrmrkbaajpwmss4rg0mak82k16d6588w9ncaffmbrnl"))

(define rust-tower-layer-0.3.3
  (crate-source "tower-layer" "0.3.3"
                "03kq92fdzxin51w8iqix06dcfgydyvx7yr6izjq0p626v9n2l70j"))

(define rust-tower-service-0.3.3
  (crate-source "tower-service" "0.3.3"
                "1hzfkvkci33ra94xjx64vv3pp0sq346w06fpkcdwjcid7zhvdycd"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-serde-0.2.0
  (crate-source "tracing-serde" "0.2.0"
                "1wbgzi364vzfswfkvy48a3p0z5xmv98sx342r57sil70ggmiljvh"))

(define rust-tracing-subscriber-0.3.23
  (crate-source "tracing-subscriber" "0.3.23"
                "06fkr0qhggvrs861d7f74pn3i3a10h5jsp4n70jj9ys5b675fzyb"))

(define rust-tracing-tracy-0.11.4
  (crate-source "tracing-tracy" "0.11.4"
                "1fp4asppg1kzz44ww4961xgd1nfj1gf57ajcwklhyvm9mx91iahf"))

(define rust-tracy-client-0.18.4
  (crate-source "tracy-client" "0.18.4"
                "19g6g3s5x891k419ahl6y4xnbz100viyjwn7j2mqcpdcmqxzrxm4"))

(define rust-tracy-client-sys-0.28.0
  (crate-source "tracy-client-sys" "0.28.0"
                "1gxc1lb3yvbzb8n5069x1gis6vpfdly7n5bj7n8iq37j919wkxy5"))

(define rust-try-lock-0.2.5
  (crate-source "try-lock" "0.2.5"
                "0jqijrrvm1pyq34zn1jmy2vihd4jcrjlvsh4alkjahhssjnsn8g4"))

(define rust-type-map-0.5.1
  (crate-source "type-map" "0.5.1"
                "143v32wwgpymxfy4y8s694vyq0wdi7li4s5dmms5w59nj2yxnc6b"))

(define rust-typenum-1.20.0
  (crate-source "typenum" "1.20.0"
                "1pj35y6q11d3y55gdl6g1h2dfhmybjming0jdi9bh0bpnqm11kj0"))

(define rust-typenum-1.20.1
  (crate-source "typenum" "1.20.1"
                "086s9ly0906kw5yw41249fba97w5zfxf03pyfwdkffvcprqfixdn"))

(define rust-uds-windows-1.2.1
  (crate-source "uds_windows" "1.2.1"
                "0vidqwwfgn8wyzvbxiqil787b4wyqjia50zpdbbjqx7n8wlgpxpj"))

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

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unit-prefix-0.5.2
  (crate-source "unit-prefix" "0.5.2"
                "18xr6yhdvlxrv51y6js9npa3qhkzc5b1z4skr5kfzn7kkd449rc1"))

(define rust-untrusted-0.9.0
  (crate-source "untrusted" "0.9.0"
                "1ha7ib98vkc538x0z60gfn0fc5whqdd85mb87dvisdcaifi6vjwf"))

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

(define rust-uuid-1.24.0
  (crate-source "uuid" "1.24.0"
                "0faj5x0zgri8m3i8dv9qgyhiwqwdyhbl2g351cp3iin4ynk26fdz"))

(define rust-uuid-1.25.0
  (crate-source "uuid" "1.25.0"
                "1k5y394cmcrpl038i5szyxk96pa27nzgn89480d7cnph6ilmflzh"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-version-compare-0.2.1
  (crate-source "version-compare" "0.2.1"
                "03nziqxwnxlizl42cwsx33vi5xd2cf2jnszhh9rzay7g6xl8bhh3"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-want-0.3.1
  (crate-source "want" "0.3.1"
                "03hbfrnvqqdchb5kgxyavb9jabwza0dmh2vw5kg0dq8rxl57d9xz"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.3+wasi-0.2.9
  (crate-source "wasip2" "1.0.3+wasi-0.2.9"
                "1mi3w855dz99xzjqc4aa8c9q5b6z1y5c963pkk4cvmr6vdr4c1i0"))

(define rust-wasip2-1.0.4+wasi-0.2.12
  (crate-source "wasip2" "1.0.4+wasi-0.2.12"
                "11wl7lqwq4pbmlmzr6n7bwz0hzy1z6sxc4554bkmrr86w4vznzmn"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.118
  (crate-source "wasm-bindgen" "0.2.118"
                "129s5r14fx4v4xrzpx2c6l860nkxpl48j50y7kl6j16bpah3iy8b"))

(define rust-wasm-bindgen-0.2.127
  (crate-source "wasm-bindgen" "0.2.127"
                "0w6fa1mkbb6qlkffgy4qaz0hdf496zbjkyiyvs4lvmpd8xbr6w0v"))

(define rust-wasm-bindgen-futures-0.4.68
  (crate-source "wasm-bindgen-futures" "0.4.68"
                "1y7bq5d9fk7s9xaayx38bgs9ns35na0kpb5zw19944zvya1x6wgk"))

(define rust-wasm-bindgen-futures-0.4.77
  (crate-source "wasm-bindgen-futures" "0.4.77"
                "0l3r8m335kb2p8yj65kb0biwlypcx3ay4g750hafkl13rkapfxvb"))

(define rust-wasm-bindgen-macro-0.2.118
  (crate-source "wasm-bindgen-macro" "0.2.118"
                "1v98r8vs17cj8918qsg0xx4nlg4nxk1g0jd4nwnyrh1687w29zzf"))

(define rust-wasm-bindgen-macro-0.2.127
  (crate-source "wasm-bindgen-macro" "0.2.127"
                "1hcvlb6bv771fvgifd367wd0cm4giyar8fq5i4h705vj7y7myxvp"))

(define rust-wasm-bindgen-macro-support-0.2.118
  (crate-source "wasm-bindgen-macro-support" "0.2.118"
                "0169jr0q469hfx5zqxfyywf2h2f4aj17vn4zly02nfwqmxghc24x"))

(define rust-wasm-bindgen-macro-support-0.2.127
  (crate-source "wasm-bindgen-macro-support" "0.2.127"
                "112j4d7dv8y2sk9yy9czrl9fpjx9388ywnn7icdv2bywazw367g1"))

(define rust-wasm-bindgen-shared-0.2.118
  (crate-source "wasm-bindgen-shared" "0.2.118"
                "0ag1vvdzi4334jlzilsy14y3nyzwddf1ndn62fyhf6bg62g4vl2z"))

(define rust-wasm-bindgen-shared-0.2.127
  (crate-source "wasm-bindgen-shared" "0.2.127"
                "1gywp6xv8a27fvm3ga9xby93xyic3hc2s626b9z9rw2xqny4vxky"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-wayland-backend-0.3.16
  (crate-source "wayland-backend" "0.3.16"
                "1f2l7zw10cwid6444w86szvr08wvgkhi6a31k64nz2y5s40wyv01"))

(define rust-wayland-backend-0.3.17
  (crate-source "wayland-backend" "0.3.17"
                "0y50cw56f09cdcsinbbl94naz91xf7iqaj87s4f7py6zmm71pa9q"))

(define rust-wayland-client-0.31.15
  (crate-source "wayland-client" "0.31.15"
                "0ww0d0r6rn2h0sn8ma1f7zvxj40l6930p07j044nvmqshq7nmhz3"))

(define rust-wayland-protocols-0.32.13
  (crate-source "wayland-protocols" "0.32.13"
                "1dn4injzx1lnmacnhl3q60m743lvshxmmy0aabb2xaixvq9wil13"))

(define rust-wayland-scanner-0.31.11
  (crate-source "wayland-scanner" "0.31.11"
                "1h0al3271l2w124sxlh77s1kmjg0z24ns2mk1vbnfars3d3313ik"))

(define rust-wayland-sys-0.31.11
  (crate-source "wayland-sys" "0.31.11"
                "1gp3hlkxx13i55lyyi794vnw9a780z3skx0xhj71zr69xwzv5snq"))

(define rust-web-sys-0.3.104
  (crate-source "web-sys" "0.3.104"
                "0c0acbvaqzqf21q5vdff2g74fvb7afi91xjplmclybq4d24k6df4"))

(define rust-web-sys-0.3.95
  (crate-source "web-sys" "0.3.95"
                "0zfr2jy5bpkkggl88i43yy37p538hg20i56kwn421yj9g6qznbag"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-webpki-roots-0.25.4
  (crate-source "webpki-roots" "0.25.4"
                "1qgqa615gc1cgklls4bkjp9jv9pvv3jnl82lc6wd7dkximywa82z"))

(define rust-webpki-roots-1.0.7
  (crate-source "webpki-roots" "1.0.7"
                "17gblaqmp51znxd2c18c04k8yfnf7s77c04n6hdmzxbcr52fxxaj"))

(define rust-webpki-roots-1.0.9
  (crate-source "webpki-roots" "1.0.9"
                "0apja04243wz3vi26pqjg4sq8cqaac66prj490sgb1crlc4rvkbx"))

(define rust-whatadistro-0.1.0
  (crate-source "whatadistro" "0.1.0"
                "04yr0lnmpjmr8vhdjy5v47zinqs5fic6289z108rb02r9ynyp5qw"))

(define rust-which-4.4.2
  (crate-source "which" "4.4.2"
                "1ixzmx3svsv5hbdvd8vdhd3qwvf6ns8jdpif1wmwsy10k90j9fl7"))

(define rust-widestring-1.2.1
  (crate-source "widestring" "1.2.1"
                "0wg4qdbs70xqnlbm8wb0bs4idm2mxk3b6kaqwllsncmb2cqrq1kj"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-wincompatlib-0.7.7
  (crate-source "wincompatlib" "0.7.7"
                "016dypkxhk9qi218n8halhp5w3aik5q7w489sy9zq2vmsc01dr5a"))

(define rust-windows-0.61.3
  (crate-source "windows" "0.61.3"
                "14v8dln7i4ccskd8danzri22bkjkbmgzh284j3vaxhd4cykx7awv"))

(define rust-windows-0.62.2
  (crate-source "windows" "0.62.2"
                "10457l9ihrbw8j79z2v4plyjxkf6xvb5npd0lqwmkh702gpaszsj"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.1
  (crate-source "windows_aarch64_gnullvm" "0.53.1"
                "0lqvdm510mka9w26vmga7hbkmrw9glzc90l4gya5qbxlm1pl3n59"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.1
  (crate-source "windows_aarch64_msvc" "0.53.1"
                "01jh2adlwx043rji888b22whx4bm8alrk3khjpik5xn20kl85mxr"))

(define rust-windows-collections-0.2.0
  (crate-source "windows-collections" "0.2.0"
                "1s65anr609qvsjga7w971p6iq964h87670dkfqfypnfgwnswxviv"))

(define rust-windows-collections-0.3.2
  (crate-source "windows-collections" "0.3.2"
                "0436rjbkqn3j9m2v2lcmwwk0l3n2r57yvqb7fcy4m8d8y5ddkci3"))

(define rust-windows-core-0.61.2
  (crate-source "windows-core" "0.61.2"
                "1qsa3iw14wk4ngfl7ipcvdf9xyq456ms7cx2i9iwf406p7fx7zf0"))

(define rust-windows-core-0.62.2
  (crate-source "windows-core" "0.62.2"
                "1swxpv1a8qvn3bkxv8cn663238h2jccq35ff3nsj61jdsca3ms5q"))

(define rust-windows-future-0.2.1
  (crate-source "windows-future" "0.2.1"
                "13mdzcdn51ckpzp3frb8glnmkyjr1c30ym9wnzj9zc97hkll2spw"))

(define rust-windows-future-0.3.2
  (crate-source "windows-future" "0.3.2"
                "1jq5qs2dwzf6rl60f8gr49z2mifxsrdh4y4yfdws467ya41gkmp1"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.1
  (crate-source "windows_i686_gnu" "0.53.1"
                "18wkcm82ldyg4figcsidzwbg1pqd49jpm98crfz0j7nqd6h6s3ln"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.1
  (crate-source "windows_i686_gnullvm" "0.53.1"
                "030qaxqc4salz6l4immfb6sykc6gmhyir9wzn2w8mxj8038mjwzs"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.1
  (crate-source "windows_i686_msvc" "0.53.1"
                "1hi6scw3mn2pbdl30ji5i4y8vvspb9b66l98kkz350pig58wfyhy"))

(define rust-windows-implement-0.60.2
  (crate-source "windows-implement" "0.60.2"
                "1psxhmklzcf3wjs4b8qb42qb6znvc142cb5pa74rsyxm1822wgh5"))

(define rust-windows-interface-0.59.3
  (crate-source "windows-interface" "0.59.3"
                "0n73cwrn4247d0axrk7gjp08p34x1723483jxjxjdfkh4m56qc9z"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-numerics-0.2.0
  (crate-source "windows-numerics" "0.2.0"
                "1cf2j8nbqf0hqqa7chnyid91wxsl2m131kn0vl3mqk3c0rlayl4i"))

(define rust-windows-numerics-0.3.1
  (crate-source "windows-numerics" "0.3.1"
                "09hgbg8pf89r4090yyhh9q29ppi7yyxkgmga9ascshy19a240bkf"))

(define rust-windows-result-0.3.4
  (crate-source "windows-result" "0.3.4"
                "1il60l6idrc6hqsij0cal0mgva6n3w6gq4ziban8wv6c6b9jpx2n"))

(define rust-windows-result-0.4.1
  (crate-source "windows-result" "0.4.1"
                "1d9yhmrmmfqh56zlj751s5wfm9a2aa7az9rd7nn5027nxa4zm0bp"))

(define rust-windows-strings-0.4.2
  (crate-source "windows-strings" "0.4.2"
                "0mrv3plibkla4v5kaakc2rfksdd0b14plcmidhbkcfqc78zwkrjn"))

(define rust-windows-strings-0.5.1
  (crate-source "windows-strings" "0.5.1"
                "14bhng9jqv4fyl7lqjz3az7vzh8pw0w4am49fsqgcz67d67x0dvq"))

(define rust-windows-sys-0.52.0
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.2
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.5
  (crate-source "windows-targets" "0.53.5"
                "1wv9j2gv3l6wj3gkw5j1kr6ymb5q6dfc42yvydjhv3mqa7szjia9"))

(define rust-windows-threading-0.1.0
  (crate-source "windows-threading" "0.1.0"
                "19jpn37zpjj2q7pn07dpq0ay300w65qx7wdp13wbp8qf5snn6r5n"))

(define rust-windows-threading-0.2.1
  (crate-source "windows-threading" "0.2.1"
                "0dsvsy33vxs0153z4n39sqkzx382cjjkrd46rb3z3zfak5dvsj9r"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.1
  (crate-source "windows_x86_64_gnu" "0.53.1"
                "16d4yiysmfdlsrghndr97y57gh3kljkwhfdbcs05m1jasz6l4f4w"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.1
  (crate-source "windows_x86_64_gnullvm" "0.53.1"
                "1qbspgv4g3q0vygkg8rnql5c6z3caqv38japiynyivh75ng1gyhg"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.1
  (crate-source "windows_x86_64_msvc" "0.53.1"
                "0l6npq76vlq4ksn4bwsncpr8508mk0gmznm6wnhjg95d19gzzfyn"))

(define rust-winnow-1.0.4
  (crate-source "winnow" "1.0.4"
                "10fzxipa7lx16172p3aca9j60hzbqgjki2f95kqksd5qywcp7f93"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-0.57.1
  (crate-source "wit-bindgen" "0.57.1"
                "0vjk2jb593ri9k1aq4iqs2si9mrw5q46wxnn78im7hm7hx799gqy"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

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

(define rust-writeable-0.6.3
  (crate-source "writeable" "0.6.3"
                "1i54d13h9bpap2hf13xcry1s4lxh7ap3923g8f3c0grd7c9fbyhz"))

(define rust-writeable-0.6.4
  (crate-source "writeable" "0.6.4"
                "1p3r4s4wbf3dksfpj3xyrn7id5p0f7r74mj6qx6ngjfd6cm2vn1s"))

(define rust-xattr-1.6.1
  (crate-source "xattr" "1.6.1"
                "0ml1mb43gqasawillql6b344m0zgq8mz0isi11wj8vbg43a5mr1j"))

(define rust-xz-0.1.0
  (crate-source "xz" "0.1.0"
                "0d6sq57g1969hjl5k7gzzdbyr60za9hk8qs9iqz26biazy87d21w"))

(define rust-xz2-0.1.7
  (crate-source "xz2" "0.1.7"
                "1qk7nzpblizvayyq4xzi4b0zacmmbqr6vb9fc0v1avyp17f4931q"))

(define rust-yoke-0.8.2
  (crate-source "yoke" "0.8.2"
                "1jprcs7a98a5whvfs6r3jvfh1nnfp6zyijl7y4ywmn88lzywbs5b"))

(define rust-yoke-0.8.3
  (crate-source "yoke" "0.8.3"
                "1xgyj6c2lxj2bp891ynmhws87c6z7yyv2li1v0ss9di40hxf57vh"))

(define rust-yoke-derive-0.8.2
  (crate-source "yoke-derive" "0.8.2"
                "13l5y5sz4lqm7rmyakjbh6vwgikxiql51xfff9hq2j485hk4r16y"))

(define rust-zbus-5.19.0
  (crate-source "zbus" "5.19.0"
                "01sram5sgwsg3x8mghx77cjbsfa2c10mar7fnzj23d2w0xybxd2x"))

(define rust-zbus-macros-5.19.0
  (crate-source "zbus_macros" "5.19.0"
                "0h4gr26kyhdyn503rgg8h44sjxm8d6n8qbzpd0cdzrmd15fn7419"))

(define rust-zbus-names-4.3.4
  (crate-source "zbus_names" "4.3.4"
                "0kk250s3x1fxpz9fvhdr64ydbacpn8ah23hy021yhlzzlfs8igyq"))

(define rust-zcheapstr-1.1.0
  (crate-source "zcheapstr" "1.1.0"
                "0wwlv70bi2rydvvzfq249q6i51mjx85c4m2wxcx1hra5c18yrbyi"))

(define rust-zerocopy-0.8.48
  (crate-source "zerocopy" "0.8.48"
                "1sb8plax8jbrsng1jdval7bdhk7hhrx40dz3hwh074k6knzkgm7f"))

(define rust-zerocopy-0.8.56
  (crate-source "zerocopy" "0.8.56"
                "1svmifchgdk0sm7v24lfwhhxis57gwa2lg21ingmmd5dhgjn8rsm"))

(define rust-zerocopy-derive-0.8.48
  (crate-source "zerocopy-derive" "0.8.48"
                "1m5s0g92cxggqc74j83k1priz24k3z93sj5gadppd20p9c4cvqvh"))

(define rust-zerocopy-derive-0.8.56
  (crate-source "zerocopy-derive" "0.8.56"
                "1hfz1hfxj86y1sgyia8gbisny9bl9bwlbahg4jypjmsp43y45azj"))

(define rust-zerofrom-0.1.7
  (crate-source "zerofrom" "0.1.7"
                "1py40in4rirc9q8w36q67pld0zk8ssg024xhh0cncxgal7ra3yk9"))

(define rust-zerofrom-0.1.8
  (crate-source "zerofrom" "0.1.8"
                "0wjjdj7gdmd0iq91gzkxl7dlv0nhkk80l4bmdpzh3a1yh48mmh0f"))

(define rust-zerofrom-derive-0.1.7
  (crate-source "zerofrom-derive" "0.1.7"
                "18c4wsnznhdxx6m80piil1lbyszdiwsshgjrybqcm4b6qic22lqi"))

(define rust-zeroize-1.8.2
  (crate-source "zeroize" "1.8.2"
                "1l48zxgcv34d7kjskr610zqsm6j2b4fcr2vfh9jm9j1jgvk58wdr"))

(define rust-zeroize-1.9.0
  (crate-source "zeroize" "1.9.0"
                "0kpnij2v1ig6g2mhc0bnci0lrdfdhiq40afbc0fahajqc9jiag71"))

(define rust-zeroize-derive-1.4.3
  (crate-source "zeroize_derive" "1.4.3"
                "0bl5vd1lz27p4z336nximg5wrlw5j7jc8fxh7iv6r1wrhhav99c5"))

(define rust-zeroize-derive-1.5.0
  (crate-source "zeroize_derive" "1.5.0"
                "0a7kq8srk81pn23xqn7c9jw1jpnfy41ffn802x1zrqqgpdf6al1w"))

(define rust-zerotrie-0.2.4
  (crate-source "zerotrie" "0.2.4"
                "1gr0pkcn3qsr6in6iixqyp0vbzwf2j1jzyvh7yl2yydh3p9m548g"))

(define rust-zerotrie-0.2.5
  (crate-source "zerotrie" "0.2.5"
                "0gss16krjzk22m57dz5hkdjg99ibj6pa41qr68na7w1jpp1nk8jf"))

(define rust-zerovec-0.11.6
  (crate-source "zerovec" "0.11.6"
                "0fdjsy6b31q9i0d73sl7xjd12xadbwi45lkpfgqnmasrqg5i3ych"))

(define rust-zerovec-0.11.7
  (crate-source "zerovec" "0.11.7"
                "1n4n109wgbbin5hljq6gmbnqqg74yp9igzf40gbw2rkdjyswddcl"))

(define rust-zerovec-0.11.8
  (crate-source "zerovec" "0.11.8"
                "1n3xlvyba8riys9s8awy4xp533phqycr78nbsmvdkh86g3hn815v"))

(define rust-zerovec-derive-0.11.3
  (crate-source "zerovec-derive" "0.11.3"
                "0m85qj92mmfvhjra6ziqky5b1p4kcmp5069k7kfadp5hr8jw8pb2"))

(define rust-zerovec-derive-0.11.4
  (crate-source "zerovec-derive" "0.11.4"
                "0qmanjqvn97qwsscnzwyy7vad6h8rhydqq014gjvy0ka48ijah27"))

(define rust-zerovec-derive-0.11.6
  (crate-source "zerovec-derive" "0.11.6"
                "1ni5j8v99x3fcf3l8kp64b7aq4vf8y22jshfq74xs9mxkp1nzprl"))

(define rust-zip-3.0.0
  (crate-source "zip" "3.0.0"
                "024kvq5znpyaqggfkz2807h43m23dww1r53zc1gi1l1fa098hn8j"))

(define rust-zlib-rs-0.6.3
  (crate-source "zlib-rs" "0.6.3"
                "04qmv85amq6sv73bzqgvnlsk9mnrl97rygzf2v4zjcx1807d9qrv"))

(define rust-zlib-rs-0.6.7
  (crate-source "zlib-rs" "0.6.7"
                "04mz6314vrfwg8k91qfgs9c71c1icvixcikvki7mls4xilc1vcrl"))

(define rust-zmij-1.0.21
  (crate-source "zmij" "1.0.21"
                "1amb5i6gz7yjb0dnmz5y669674pqmwbj44p4yfxfv2ncgvk8x15q"))

(define rust-zmij-1.0.23
  (crate-source "zmij" "1.0.23"
                "06zwri21nnrl34rwinmvbciap8yk1mrl8qfg9pff7lgspc56sri9"))

(define rust-zopfli-0.8.3
  (crate-source "zopfli" "0.8.3"
                "0jaj5dyh3mks0805h4ldrsh5pwq4i2jc9dc9zwjm91k3gmwxhp7h"))

(define rust-zstd-0.13.3
  (crate-source "zstd" "0.13.3"
                "12n0h4w9l526li7jl972rxpyf012jw3nwmji2qbjghv9ll8y67p9"))

(define rust-zstd-safe-7.2.4
  (crate-source "zstd-safe" "7.2.4"
                "179vxmkzhpz6cq6mfzvgwc99bpgllkr6lwxq7ylh5dmby3aw8jcg"))

(define rust-zstd-sys-2.0.16+zstd.1.5.7
  (crate-source "zstd-sys" "2.0.16+zstd.1.5.7"
                "0j1pd2iaqpvaxlgqmmijj68wma7xwdv9grrr63j873yw5ay9xqci"))

(define rust-zvariant-5.14.0
  (crate-source "zvariant" "5.14.0"
                "0p0v7ggyzsim1s5zgbbkg6dfxfanq786aw1y3xddmf4bpljqrqmm"))

(define rust-zvariant-5.15.0
  (crate-source "zvariant" "5.15.0"
                "0iwihslxshfhalihp6kv7xz7nbv1p3b9sl97hi2izpbcrhklrly1"))

(define rust-zvariant-derive-5.14.0
  (crate-source "zvariant_derive" "5.14.0"
                "15kz8x17rxv772r02dhyspchmdpnfxry9n9b4dzbd0sjd12s35nl"))

(define rust-zvariant-derive-5.15.0
  (crate-source "zvariant_derive" "5.15.0"
                "15y4z1rkcpvrz7dv7j2rfv8wiq6i8nzifj9pgw6dnlj3kgk5ahc6"))

(define rust-zvariant-utils-4.0.0
  (crate-source "zvariant_utils" "4.0.0"
                "1mn2ygccznrcvhdk919vrxm1day4jgj1nx78w07x5ji2wbn817b2"))

(define rust-zvariant-utils-4.2.0
  (crate-source "zvariant_utils" "4.2.0"
                "18q80094ci64myzvcp0g2l3c6mnx7b3hsii8lfabc853c51jkl5s"))

(define ssss-separator 'end-of-crates)

;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs
                     (an-anime-game-launcher =>
                                             (list rust-addr2line-0.25.1
                                              rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.5
                                              rust-allocator-api2-0.2.21
                                              rust-anime-game-core-1.38.8.de96f35
                                              rust-anime-launcher-sdk-1.35.10.c0991af
                                              rust-anstream-1.0.0
                                              rust-anstyle-1.0.14
                                              rust-anstyle-parse-1.0.0
                                              rust-anstyle-query-1.1.5
                                              rust-anstyle-wincon-3.0.11
                                              rust-anyhow-1.0.104
                                              rust-arbitrary-1.4.2
                                              rust-arrayref-0.3.9
                                              rust-arrayvec-0.7.8
                                              rust-ashpd-0.11.1
                                              rust-async-broadcast-0.7.2
                                              rust-async-recursion-1.1.1
                                              rust-async-trait-0.1.92
                                              rust-atomic-waker-1.1.2
                                              rust-autocfg-1.5.1
                                              rust-backtrace-0.3.76
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.13.1
                                              rust-blake3-1.8.6
                                              rust-block-buffer-0.10.4
                                              rust-block2-0.6.2
                                              rust-bstr-1.13.1
                                              rust-bumpalo-3.20.3
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.12.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cairo-rs-0.20.12
                                              rust-cairo-sys-rs-0.20.10
                                              rust-cc-1.4.2
                                              rust-cfg-expr-0.20.8
                                              rust-cfg-if-1.0.4
                                              rust-cfg-aliases-0.2.2
                                              rust-chacha20-0.10.1
                                              rust-cipher-0.4.4
                                              rust-colorchoice-1.0.5
                                              rust-constant-time-eq-0.3.1
                                              rust-constant-time-eq-0.4.2
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-cpufeatures-0.3.0
                                              rust-crc-3.4.0
                                              rust-crc-catalog-2.5.0
                                              rust-crc32fast-1.5.0
                                              rust-crossbeam-channel-0.5.16
                                              rust-crossbeam-deque-0.8.7
                                              rust-crossbeam-epoch-0.9.20
                                              rust-crossbeam-utils-0.8.22
                                              rust-crypto-common-0.1.7
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.12
                                              rust-deranged-0.5.8
                                              rust-derive-arbitrary-1.4.2
                                              rust-digest-0.10.7
                                              rust-dispatch2-0.3.1
                                              rust-displaydoc-0.2.7
                                              rust-dlib-0.5.3
                                              rust-dns-lookup-2.1.1
                                              rust-doctest-file-1.1.1
                                              rust-downcast-rs-1.2.1
                                              rust-either-1.17.0
                                              rust-endi-1.1.1
                                              rust-enum-ordinalize-4.4.2
                                              rust-enum-ordinalize-derive-4.4.2
                                              rust-enumflags2-0.7.12
                                              rust-enumflags2-derive-0.7.12
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.14
                                              rust-event-listener-5.4.2
                                              rust-event-listener-strategy-0.5.4
                                              rust-fastrand-2.5.0
                                              rust-field-offset-0.3.6
                                              rust-filetime-0.2.29
                                              rust-find-msvc-tools-0.1.10
                                              rust-fixedbitset-0.5.7
                                              rust-flate2-1.1.9
                                              rust-fluent-bundle-0.16.0
                                              rust-fluent-langneg-0.13.1
                                              rust-fluent-syntax-0.12.0
                                              rust-fluent-template-macros-0.13.3
                                              rust-fluent-templates-0.13.3
                                              rust-flume-0.11.1
                                              rust-fnv-1.0.7
                                              rust-foldhash-0.1.5
                                              rust-form-urlencoded-1.2.2
                                              rust-fragile-2.1.0
                                              rust-fs2-0.4.3
                                              rust-fs-extra-1.3.0
                                              rust-futures-0.3.34
                                              rust-futures-channel-0.3.34
                                              rust-futures-core-0.3.34
                                              rust-futures-executor-0.3.34
                                              rust-futures-io-0.3.34
                                              rust-futures-lite-2.6.1
                                              rust-futures-macro-0.3.34
                                              rust-futures-sink-0.3.34
                                              rust-futures-task-0.3.34
                                              rust-futures-util-0.3.34
                                              rust-gdk-pixbuf-0.20.10
                                              rust-gdk-pixbuf-sys-0.20.10
                                              rust-gdk4-0.9.6
                                              rust-gdk4-sys-0.9.6
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.17
                                              rust-getrandom-0.3.4
                                              rust-getrandom-0.4.3
                                              rust-gimli-0.32.3
                                              rust-gio-0.20.12
                                              rust-gio-sys-0.20.10
                                              rust-glib-0.20.12
                                              rust-glib-build-tools-0.20.0
                                              rust-glib-macros-0.20.12
                                              rust-glib-sys-0.20.10
                                              rust-globset-0.4.20
                                              rust-gobject-sys-0.20.10
                                              rust-graphene-rs-0.20.10
                                              rust-graphene-sys-0.20.10
                                              rust-gsk4-0.9.6
                                              rust-gsk4-sys-0.9.6
                                              rust-gtk4-0.9.7
                                              rust-gtk4-macros-0.9.5
                                              rust-gtk4-sys-0.9.6
                                              rust-h2-0.4.15
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.5
                                              rust-hashbrown-0.17.1
                                              rust-heck-0.5.0
                                              rust-hex-0.4.3
                                              rust-hmac-0.12.1
                                              rust-http-1.5.0
                                              rust-http-body-1.1.0
                                              rust-http-body-util-0.1.5
                                              rust-httparse-1.10.1
                                              rust-human-panic-2.0.8
                                              rust-hyper-1.11.0
                                              rust-hyper-rustls-0.27.9
                                              rust-hyper-util-0.1.20
                                              rust-icu-collections-2.3.0
                                              rust-icu-locale-core-2.3.0
                                              rust-icu-normalizer-2.3.0
                                              rust-icu-normalizer-data-2.3.0
                                              rust-icu-properties-2.3.0
                                              rust-icu-properties-data-2.3.0
                                              rust-icu-provider-2.3.0
                                              rust-ident-case-1.0.1
                                              rust-idna-1.1.0
                                              rust-idna-adapter-1.2.2
                                              rust-ignore-0.4.33
                                              rust-indexmap-2.14.0
                                              rust-inout-0.1.4
                                              rust-interprocess-2.4.3
                                              rust-intl-memoizer-0.5.3
                                              rust-intl-pluralrules-7.0.2
                                              rust-ipnet-2.12.1
                                              rust-is-docker-0.2.0
                                              rust-is-wsl-0.4.0
                                              rust-is-terminal-polyfill-1.70.2
                                              rust-itertools-0.14.0
                                              rust-itoa-1.0.18
                                              rust-jobserver-0.1.35
                                              rust-js-sys-0.3.104
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-libadwaita-0.7.2
                                              rust-libadwaita-sys-0.7.2
                                              rust-libc-0.2.189
                                              rust-libloading-0.8.9
                                              rust-linux-raw-sys-0.12.1
                                              rust-litemap-0.8.3
                                              rust-lock-api-0.4.14
                                              rust-log-0.4.33
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.8.3
                                              rust-memoffset-0.9.1
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.1
                                              rust-mio-1.2.2
                                              rust-multimap-0.10.1
                                              rust-nanorand-0.7.0
                                              rust-ntapi-0.4.3
                                              rust-nu-ansi-term-0.50.3
                                              rust-num-conv-0.2.2
                                              rust-objc2-0.6.4
                                              rust-objc2-app-kit-0.3.2
                                              rust-objc2-core-foundation-0.3.2
                                              rust-objc2-encode-4.1.0
                                              rust-objc2-foundation-0.3.2
                                              rust-objc2-io-kit-0.3.2
                                              rust-object-0.37.3
                                              rust-once-cell-1.21.4
                                              rust-once-cell-polyfill-1.70.2
                                              rust-open-5.4.1
                                              rust-openssl-probe-0.1.6
                                              rust-ordered-stream-0.2.0
                                              rust-pango-0.20.12
                                              rust-pango-sys-0.20.10
                                              rust-parking-2.2.1
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.2
                                              rust-petgraph-0.8.3
                                              rust-pin-project-lite-0.2.17
                                              rust-pkg-config-0.3.34
                                              rust-pollster-0.4.0
                                              rust-potential-utf-0.1.6
                                              rust-powerfmt-0.2.0
                                              rust-ppv-lite86-0.2.21
                                              rust-prettyplease-0.2.37
                                              rust-proc-macro-crate-3.5.0
                                              rust-proc-macro-hack-0.5.20+deprecated
                                              rust-proc-macro2-1.0.107
                                              rust-prost-0.14.4
                                              rust-prost-build-0.14.4
                                              rust-prost-derive-0.14.4
                                              rust-prost-types-0.14.4
                                              rust-quick-xml-0.41.0
                                              rust-quinn-0.11.11
                                              rust-quinn-proto-0.11.16
                                              rust-quinn-udp-0.5.15
                                              rust-quote-1.0.47
                                              rust-r-efi-5.3.0
                                              rust-r-efi-6.0.0
                                              rust-rand-0.9.5
                                              rust-rand-0.10.2
                                              rust-rand-chacha-0.9.0
                                              rust-rand-core-0.9.5
                                              rust-rand-core-0.10.1
                                              rust-rand-pcg-0.10.2
                                              rust-raw-window-handle-0.6.2
                                              rust-recvmsg-1.0.0
                                              rust-regex-1.13.1
                                              rust-regex-automata-0.4.18
                                              rust-regex-syntax-0.8.11
                                              rust-relm4-0.9.1
                                              rust-relm4-css-0.9.0
                                              rust-relm4-macros-0.9.1
                                              rust-reqwest-0.12.28
                                              rust-rfd-0.15.4
                                              rust-ring-0.17.14
                                              rust-rustc-demangle-0.1.28
                                              rust-rustc-hash-2.1.3
                                              rust-rustc-version-0.4.1
                                              rust-rustix-1.1.4
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.43
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.15.1
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.14
                                              rust-rustversion-1.0.23
                                              rust-ryu-1.0.23
                                              rust-same-file-1.0.6
                                              rust-schannel-0.1.29
                                              rust-scoped-tls-1.0.1
                                              rust-scopeguard-1.2.0
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.17.0
                                              rust-self-cell-1.3.0
                                              rust-semver-1.0.28
                                              rust-serde-1.0.229
                                              rust-serde-core-1.0.229
                                              rust-serde-derive-1.0.229
                                              rust-serde-json-1.0.151
                                              rust-serde-repr-0.1.21
                                              rust-serde-spanned-1.1.1
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.7
                                              rust-sharded-slab-0.1.7
                                              rust-shlex-2.0.1
                                              rust-signal-hook-registry-1.4.8
                                              rust-simd-adler32-0.3.10
                                              rust-slab-0.4.12
                                              rust-smallvec-1.15.2
                                              rust-socket2-0.6.5
                                              rust-sophon-lib-0.1.6.89f4a70
                                              rust-spin-0.9.9
                                              rust-stable-deref-trait-1.2.1
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.119
                                              rust-syn-3.0.3
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-sysinfo-0.38.4
                                              rust-system-deps-7.0.8
                                              rust-tar-0.4.46
                                              rust-target-lexicon-0.13.5
                                              rust-tempfile-3.27.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.20
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.20
                                              rust-thread-local-1.1.10
                                              rust-time-0.3.55
                                              rust-time-core-0.1.9
                                              rust-tinystr-0.8.4
                                              rust-tinyvec-1.12.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.53.1
                                              rust-tokio-rustls-0.26.4
                                              rust-tokio-util-0.7.19
                                              rust-toml-1.1.4+spec-1.1.0
                                              rust-toml-datetime-1.1.1+spec-1.1.0
                                              rust-toml-edit-0.25.13+spec-1.1.0
                                              rust-toml-parser-1.1.3+spec-1.1.0
                                              rust-toml-writer-1.1.2+spec-1.1.0
                                              rust-tower-0.5.3
                                              rust-tower-http-0.6.11
                                              rust-tower-layer-0.3.3
                                              rust-tower-service-0.3.3
                                              rust-tracing-0.1.44
                                              rust-tracing-attributes-0.1.31
                                              rust-tracing-core-0.1.36
                                              rust-tracing-log-0.2.0
                                              rust-tracing-subscriber-0.3.23
                                              rust-try-lock-0.2.5
                                              rust-type-map-0.5.1
                                              rust-typenum-1.20.1
                                              rust-uds-windows-1.2.1
                                              rust-unic-langid-0.9.6
                                              rust-unic-langid-impl-0.9.6
                                              rust-unic-langid-macros-0.9.6
                                              rust-unic-langid-macros-impl-0.9.6
                                              rust-unicode-ident-1.0.24
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.8
                                              rust-urlencoding-2.1.3
                                              rust-utf8-iter-1.0.4
                                              rust-utf8parse-0.2.2
                                              rust-uuid-1.24.0
                                              rust-valuable-0.1.1
                                              rust-version-compare-0.2.1
                                              rust-version-check-0.9.5
                                              rust-walkdir-2.5.0
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasip2-1.0.4+wasi-0.2.12
                                              rust-wasm-bindgen-0.2.127
                                              rust-wasm-bindgen-futures-0.4.77
                                              rust-wasm-bindgen-macro-0.2.127
                                              rust-wasm-bindgen-macro-support-0.2.127
                                              rust-wasm-bindgen-shared-0.2.127
                                              rust-wayland-backend-0.3.16
                                              rust-wayland-client-0.31.15
                                              rust-wayland-protocols-0.32.13
                                              rust-wayland-scanner-0.31.11
                                              rust-wayland-sys-0.31.11
                                              rust-web-sys-0.3.104
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-0.25.4
                                              rust-webpki-roots-1.0.9
                                              rust-whatadistro-0.1.0
                                              rust-widestring-1.2.1
                                              rust-winapi-0.3.9
                                              rust-winapi-i686-pc-windows-gnu-0.4.0
                                              rust-winapi-util-0.1.11
                                              rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                              rust-wincompatlib-0.7.7
                                              rust-windows-0.61.3
                                              rust-windows-0.62.2
                                              rust-windows-collections-0.2.0
                                              rust-windows-collections-0.3.2
                                              rust-windows-core-0.61.2
                                              rust-windows-core-0.62.2
                                              rust-windows-future-0.2.1
                                              rust-windows-future-0.3.2
                                              rust-windows-implement-0.60.2
                                              rust-windows-interface-0.59.3
                                              rust-windows-link-0.1.3
                                              rust-windows-link-0.2.1
                                              rust-windows-numerics-0.2.0
                                              rust-windows-numerics-0.3.1
                                              rust-windows-result-0.3.4
                                              rust-windows-result-0.4.1
                                              rust-windows-strings-0.4.2
                                              rust-windows-strings-0.5.1
                                              rust-windows-sys-0.52.0
                                              rust-windows-sys-0.59.0
                                              rust-windows-sys-0.60.2
                                              rust-windows-sys-0.61.2
                                              rust-windows-targets-0.52.6
                                              rust-windows-targets-0.53.5
                                              rust-windows-threading-0.1.0
                                              rust-windows-threading-0.2.1
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
                                              rust-winnow-1.0.4
                                              rust-wit-bindgen-0.57.1
                                              rust-writeable-0.6.4
                                              rust-xattr-1.6.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.3
                                              rust-yoke-derive-0.8.2
                                              rust-zbus-5.19.0
                                              rust-zbus-macros-5.19.0
                                              rust-zbus-names-4.3.4
                                              rust-zcheapstr-1.1.0
                                              rust-zerocopy-0.8.56
                                              rust-zerocopy-derive-0.8.56
                                              rust-zerofrom-0.1.8
                                              rust-zerofrom-derive-0.1.7
                                              rust-zeroize-1.9.0
                                              rust-zeroize-derive-1.5.0
                                              rust-zerotrie-0.2.5
                                              rust-zerovec-0.11.7
                                              rust-zerovec-derive-0.11.4
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.6.7
                                              rust-zmij-1.0.23
                                              rust-zopfli-0.8.3
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.16+zstd.1.5.7
                                              rust-zvariant-5.14.0
                                              rust-zvariant-derive-5.14.0
                                              rust-zvariant-utils-4.0.0))
                     (anime-game-core-1.36.3 =>
                                             (list rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.4
                                              rust-allocator-api2-0.2.21
                                              rust-anyhow-1.0.102
                                              rust-arbitrary-1.4.2
                                              rust-atomic-waker-1.1.2
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.11.1
                                              rust-block-buffer-0.10.4
                                              rust-bumpalo-3.20.2
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.11.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cc-1.2.61
                                              rust-cfg-if-1.0.4
                                              rust-cfg-aliases-0.2.1
                                              rust-cipher-0.4.4
                                              rust-constant-time-eq-0.3.1
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-crc-3.4.0
                                              rust-crc-catalog-2.5.0
                                              rust-crc32fast-1.5.0
                                              rust-crossbeam-deque-0.8.6
                                              rust-crossbeam-epoch-0.9.18
                                              rust-crossbeam-utils-0.8.21
                                              rust-crypto-common-0.1.7
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.12
                                              rust-deranged-0.5.8
                                              rust-derive-arbitrary-1.4.2
                                              rust-digest-0.10.7
                                              rust-displaydoc-0.2.5
                                              rust-dns-lookup-2.1.1
                                              rust-either-1.15.0
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.14
                                              rust-fastrand-2.4.1
                                              rust-filetime-0.2.27
                                              rust-find-msvc-tools-0.1.9
                                              rust-flate2-1.1.9
                                              rust-fnv-1.0.7
                                              rust-foldhash-0.1.5
                                              rust-form-urlencoded-1.2.2
                                              rust-fs-extra-1.3.0
                                              rust-futures-channel-0.3.32
                                              rust-futures-core-0.3.32
                                              rust-futures-io-0.3.32
                                              rust-futures-sink-0.3.32
                                              rust-futures-task-0.3.32
                                              rust-futures-util-0.3.32
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.17
                                              rust-getrandom-0.3.4
                                              rust-getrandom-0.4.2
                                              rust-h2-0.4.13
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.5
                                              rust-hashbrown-0.17.0
                                              rust-heck-0.5.0
                                              rust-hmac-0.12.1
                                              rust-home-0.5.12
                                              rust-http-1.4.0
                                              rust-http-body-1.0.1
                                              rust-http-body-util-0.1.3
                                              rust-httparse-1.10.1
                                              rust-hyper-1.9.0
                                              rust-hyper-rustls-0.27.9
                                              rust-hyper-util-0.1.20
                                              rust-icu-collections-2.2.0
                                              rust-icu-locale-core-2.2.0
                                              rust-icu-normalizer-2.2.0
                                              rust-icu-normalizer-data-2.2.0
                                              rust-icu-properties-2.2.0
                                              rust-icu-properties-data-2.2.0
                                              rust-icu-provider-2.2.0
                                              rust-id-arena-2.3.0
                                              rust-ident-case-1.0.1
                                              rust-idna-1.1.0
                                              rust-idna-adapter-1.2.2
                                              rust-indexmap-2.14.0
                                              rust-inout-0.1.4
                                              rust-ipnet-2.12.0
                                              rust-iri-string-0.7.12
                                              rust-itoa-1.0.18
                                              rust-jobserver-0.1.34
                                              rust-js-sys-0.3.95
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-leb128fmt-0.1.0
                                              rust-libc-0.2.186
                                              rust-libredox-0.1.16
                                              rust-linux-raw-sys-0.4.15
                                              rust-linux-raw-sys-0.12.1
                                              rust-litemap-0.8.2
                                              rust-log-0.4.29
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.8.0
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.1
                                              rust-mio-1.2.0
                                              rust-ntapi-0.4.3
                                              rust-num-conv-0.2.1
                                              rust-objc2-core-foundation-0.3.2
                                              rust-objc2-io-kit-0.3.2
                                              rust-once-cell-1.21.4
                                              rust-openssl-probe-0.1.6
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.2
                                              rust-pin-project-lite-0.2.17
                                              rust-pkg-config-0.3.33
                                              rust-plain-0.2.3
                                              rust-potential-utf-0.1.5
                                              rust-powerfmt-0.2.0
                                              rust-ppv-lite86-0.2.21
                                              rust-prettyplease-0.2.37
                                              rust-proc-macro2-1.0.106
                                              rust-protobuf-3.7.2
                                              rust-protobuf-codegen-3.7.2
                                              rust-protobuf-parse-3.7.2
                                              rust-protobuf-support-3.7.2
                                              rust-quinn-0.11.9
                                              rust-quinn-proto-0.11.14
                                              rust-quinn-udp-0.5.14
                                              rust-quote-1.0.45
                                              rust-r-efi-5.3.0
                                              rust-r-efi-6.0.0
                                              rust-rand-0.9.4
                                              rust-rand-chacha-0.9.0
                                              rust-rand-core-0.9.5
                                              rust-redox-syscall-0.7.4
                                              rust-regex-1.12.3
                                              rust-regex-automata-0.4.14
                                              rust-regex-syntax-0.8.10
                                              rust-reqwest-0.12.28
                                              rust-ring-0.17.14
                                              rust-rustc-hash-2.1.2
                                              rust-rustix-0.38.44
                                              rust-rustix-1.1.4
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.39
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.14.1
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.13
                                              rust-rustversion-1.0.22
                                              rust-ryu-1.0.23
                                              rust-schannel-0.1.29
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.17.0
                                              rust-semver-1.0.28
                                              rust-serde-1.0.228
                                              rust-serde-core-1.0.228
                                              rust-serde-derive-1.0.228
                                              rust-serde-json-1.0.149
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.6
                                              rust-shlex-1.3.0
                                              rust-simd-adler32-0.3.9
                                              rust-slab-0.4.12
                                              rust-smallvec-1.15.1
                                              rust-socket2-0.6.3
                                              rust-stable-deref-trait-1.2.1
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.117
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-tar-0.4.45
                                              rust-tempfile-3.27.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.18
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.18
                                              rust-time-0.3.47
                                              rust-time-core-0.1.8
                                              rust-tinystr-0.8.3
                                              rust-tinyvec-1.11.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.52.1
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
                                              rust-typenum-1.20.0
                                              rust-unicode-ident-1.0.24
                                              rust-unicode-xid-0.2.6
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.8
                                              rust-utf8-iter-1.0.4
                                              rust-version-check-0.9.5
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasip2-1.0.3+wasi-0.2.9
                                              rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                              rust-wasm-bindgen-0.2.118
                                              rust-wasm-bindgen-futures-0.4.68
                                              rust-wasm-bindgen-macro-0.2.118
                                              rust-wasm-bindgen-macro-support-0.2.118
                                              rust-wasm-bindgen-shared-0.2.118
                                              rust-wasm-encoder-0.244.0
                                              rust-wasm-metadata-0.244.0
                                              rust-wasmparser-0.244.0
                                              rust-web-sys-0.3.95
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-1.0.7
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
                                              rust-wit-bindgen-0.57.1
                                              rust-wit-bindgen-core-0.51.0
                                              rust-wit-bindgen-rust-0.51.0
                                              rust-wit-bindgen-rust-macro-0.51.0
                                              rust-wit-component-0.244.0
                                              rust-wit-parser-0.244.0
                                              rust-writeable-0.6.3
                                              rust-xattr-1.6.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.2
                                              rust-yoke-derive-0.8.2
                                              rust-zerocopy-0.8.48
                                              rust-zerocopy-derive-0.8.48
                                              rust-zerofrom-0.1.7
                                              rust-zerofrom-derive-0.1.7
                                              rust-zeroize-1.8.2
                                              rust-zeroize-derive-1.4.3
                                              rust-zerotrie-0.2.4
                                              rust-zerovec-0.11.6
                                              rust-zerovec-derive-0.11.3
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.6.3
                                              rust-zmij-1.0.21
                                              rust-zopfli-0.8.3
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-game-core-1.38.10 =>
                                              (list rust-adler2-2.0.1
                                               rust-aes-0.8.4
                                               rust-ahash-0.8.12
                                               rust-aho-corasick-1.1.5
                                               rust-allocator-api2-0.2.21
                                               rust-anyhow-1.0.104
                                               rust-arbitrary-1.4.2
                                               rust-atomic-waker-1.1.2
                                               rust-base64-0.21.7
                                               rust-base64-0.22.1
                                               rust-bitflags-2.13.1
                                               rust-block-buffer-0.10.4
                                               rust-bumpalo-3.20.3
                                               rust-byteorder-1.5.0
                                               rust-bytes-1.12.1
                                               rust-bzip2-0.4.4
                                               rust-bzip2-0.5.2
                                               rust-bzip2-sys-0.1.13+1.0.8
                                               rust-cached-0.55.1
                                               rust-cached-proc-macro-0.24.0
                                               rust-cached-proc-macro-types-0.1.1
                                               rust-cc-1.4.2
                                               rust-cfg-if-1.0.4
                                               rust-cfg-aliases-0.2.2
                                               rust-chacha20-0.10.1
                                               rust-cipher-0.4.4
                                               rust-constant-time-eq-0.3.1
                                               rust-core-foundation-0.9.4
                                               rust-core-foundation-sys-0.8.7
                                               rust-cpufeatures-0.2.17
                                               rust-cpufeatures-0.3.0
                                               rust-crc-3.4.0
                                               rust-crc-catalog-2.5.0
                                               rust-crc32fast-1.5.0
                                               rust-crossbeam-channel-0.5.16
                                               rust-crossbeam-utils-0.8.22
                                               rust-crypto-common-0.1.7
                                               rust-darling-0.20.11
                                               rust-darling-core-0.20.11
                                               rust-darling-macro-0.20.11
                                               rust-deflate64-0.1.12
                                               rust-deranged-0.5.8
                                               rust-derive-arbitrary-1.4.2
                                               rust-digest-0.10.7
                                               rust-displaydoc-0.2.7
                                               rust-dns-lookup-2.1.1
                                               rust-doctest-file-1.1.1
                                               rust-either-1.17.0
                                               rust-equivalent-1.0.2
                                               rust-errno-0.3.14
                                               rust-fastrand-2.5.0
                                               rust-filetime-0.2.29
                                               rust-find-msvc-tools-0.1.10
                                               rust-fixedbitset-0.5.7
                                               rust-flate2-1.1.9
                                               rust-fnv-1.0.7
                                               rust-foldhash-0.1.5
                                               rust-form-urlencoded-1.2.2
                                               rust-fs2-0.4.3
                                               rust-fs-extra-1.3.0
                                               rust-futures-channel-0.3.34
                                               rust-futures-core-0.3.34
                                               rust-futures-io-0.3.34
                                               rust-futures-sink-0.3.34
                                               rust-futures-task-0.3.34
                                               rust-futures-util-0.3.34
                                               rust-generic-array-0.14.7
                                               rust-getrandom-0.2.17
                                               rust-getrandom-0.3.4
                                               rust-getrandom-0.4.3
                                               rust-h2-0.4.15
                                               rust-hashbrown-0.14.5
                                               rust-hashbrown-0.15.5
                                               rust-hashbrown-0.17.1
                                               rust-heck-0.5.0
                                               rust-hmac-0.12.1
                                               rust-home-0.5.12
                                               rust-http-1.5.0
                                               rust-http-body-1.1.0
                                               rust-http-body-util-0.1.5
                                               rust-httparse-1.10.1
                                               rust-hyper-1.11.0
                                               rust-hyper-rustls-0.27.9
                                               rust-hyper-util-0.1.20
                                               rust-icu-collections-2.3.0
                                               rust-icu-locale-core-2.3.0
                                               rust-icu-normalizer-2.3.0
                                               rust-icu-normalizer-data-2.3.0
                                               rust-icu-properties-2.3.0
                                               rust-icu-properties-data-2.3.0
                                               rust-icu-provider-2.3.0
                                               rust-ident-case-1.0.1
                                               rust-idna-1.1.0
                                               rust-idna-adapter-1.2.2
                                               rust-indexmap-2.14.0
                                               rust-inout-0.1.4
                                               rust-interprocess-2.4.3
                                               rust-ipnet-2.12.1
                                               rust-itertools-0.14.0
                                               rust-itoa-1.0.18
                                               rust-jobserver-0.1.35
                                               rust-js-sys-0.3.104
                                               rust-kinda-virtual-fs-0.1.1
                                               rust-lazy-static-1.5.0
                                               rust-libc-0.2.189
                                               rust-linux-raw-sys-0.4.15
                                               rust-linux-raw-sys-0.12.1
                                               rust-litemap-0.8.3
                                               rust-log-0.4.33
                                               rust-lru-slab-0.1.2
                                               rust-lzma-rs-0.3.0
                                               rust-lzma-sys-0.1.20
                                               rust-md-5-0.10.6
                                               rust-md5-asm-0.5.2
                                               rust-memchr-2.8.3
                                               rust-miniz-oxide-0.8.9
                                               rust-minreq-2.14.1
                                               rust-mio-1.2.2
                                               rust-multimap-0.10.1
                                               rust-ntapi-0.4.3
                                               rust-num-conv-0.2.2
                                               rust-objc2-core-foundation-0.3.2
                                               rust-objc2-io-kit-0.3.2
                                               rust-once-cell-1.21.4
                                               rust-openssl-probe-0.1.6
                                               rust-pbkdf2-0.12.2
                                               rust-percent-encoding-2.3.2
                                               rust-petgraph-0.8.3
                                               rust-pin-project-lite-0.2.17
                                               rust-pkg-config-0.3.34
                                               rust-potential-utf-0.1.6
                                               rust-powerfmt-0.2.0
                                               rust-prettyplease-0.2.37
                                               rust-proc-macro2-1.0.107
                                               rust-prost-0.14.4
                                               rust-prost-build-0.14.4
                                               rust-prost-derive-0.14.4
                                               rust-prost-types-0.14.4
                                               rust-protobuf-3.7.2
                                               rust-protobuf-codegen-3.7.2
                                               rust-protobuf-parse-3.7.2
                                               rust-protobuf-support-3.7.2
                                               rust-quinn-0.11.11
                                               rust-quinn-proto-0.11.16
                                               rust-quinn-udp-0.5.15
                                               rust-quote-1.0.47
                                               rust-r-efi-5.3.0
                                               rust-r-efi-6.0.0
                                               rust-rand-0.10.2
                                               rust-rand-core-0.10.1
                                               rust-rand-pcg-0.10.2
                                               rust-recvmsg-1.0.0
                                               rust-regex-1.13.1
                                               rust-regex-automata-0.4.18
                                               rust-regex-syntax-0.8.11
                                               rust-reqwest-0.12.28
                                               rust-ring-0.17.14
                                               rust-rustc-hash-2.1.3
                                               rust-rustix-0.38.44
                                               rust-rustix-1.1.4
                                               rust-rustls-0.21.12
                                               rust-rustls-0.23.43
                                               rust-rustls-native-certs-0.6.3
                                               rust-rustls-pemfile-1.0.4
                                               rust-rustls-pki-types-1.15.1
                                               rust-rustls-webpki-0.101.7
                                               rust-rustls-webpki-0.103.14
                                               rust-rustversion-1.0.23
                                               rust-ryu-1.0.23
                                               rust-schannel-0.1.29
                                               rust-sct-0.7.1
                                               rust-security-framework-2.11.1
                                               rust-security-framework-sys-2.17.0
                                               rust-serde-1.0.229
                                               rust-serde-core-1.0.229
                                               rust-serde-derive-1.0.229
                                               rust-serde-json-1.0.151
                                               rust-serde-urlencoded-0.7.1
                                               rust-sha1-0.10.7
                                               rust-shlex-2.0.1
                                               rust-simd-adler32-0.3.10
                                               rust-slab-0.4.12
                                               rust-smallvec-1.15.2
                                               rust-socket2-0.6.5
                                               rust-sophon-lib-0.1.8.58d223a
                                               rust-stable-deref-trait-1.2.1
                                               rust-strsim-0.11.1
                                               rust-subtle-2.6.1
                                               rust-syn-2.0.119
                                               rust-syn-3.0.3
                                               rust-sync-wrapper-1.0.2
                                               rust-synstructure-0.13.2
                                               rust-sysinfo-0.35.2
                                               rust-tar-0.4.46
                                               rust-tempfile-3.27.0
                                               rust-thiserror-1.0.69
                                               rust-thiserror-2.0.20
                                               rust-thiserror-impl-1.0.69
                                               rust-thiserror-impl-2.0.20
                                               rust-time-0.3.55
                                               rust-time-core-0.1.9
                                               rust-tinystr-0.8.4
                                               rust-tinyvec-1.12.0
                                               rust-tinyvec-macros-0.1.1
                                               rust-tokio-1.53.1
                                               rust-tokio-rustls-0.26.4
                                               rust-tokio-util-0.7.19
                                               rust-tower-0.5.3
                                               rust-tower-http-0.6.11
                                               rust-tower-layer-0.3.3
                                               rust-tower-service-0.3.3
                                               rust-tracing-0.1.44
                                               rust-tracing-attributes-0.1.31
                                               rust-tracing-core-0.1.36
                                               rust-try-lock-0.2.5
                                               rust-typenum-1.20.1
                                               rust-unicode-ident-1.0.24
                                               rust-untrusted-0.9.0
                                               rust-url-2.5.8
                                               rust-utf8-iter-1.0.4
                                               rust-version-check-0.9.5
                                               rust-want-0.3.1
                                               rust-wasi-0.11.1+wasi-snapshot-preview1
                                               rust-wasip2-1.0.4+wasi-0.2.12
                                               rust-wasm-bindgen-0.2.127
                                               rust-wasm-bindgen-futures-0.4.77
                                               rust-wasm-bindgen-macro-0.2.127
                                               rust-wasm-bindgen-macro-support-0.2.127
                                               rust-wasm-bindgen-shared-0.2.127
                                               rust-web-sys-0.3.104
                                               rust-web-time-1.1.0
                                               rust-webpki-roots-1.0.9
                                               rust-which-4.4.2
                                               rust-widestring-1.2.1
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
                                               rust-wit-bindgen-0.57.1
                                               rust-writeable-0.6.4
                                               rust-xattr-1.6.1
                                               rust-xz-0.1.0
                                               rust-xz2-0.1.7
                                               rust-yoke-0.8.3
                                               rust-yoke-derive-0.8.2
                                               rust-zerocopy-0.8.56
                                               rust-zerocopy-derive-0.8.56
                                               rust-zerofrom-0.1.8
                                               rust-zerofrom-derive-0.1.7
                                               rust-zeroize-1.9.0
                                               rust-zeroize-derive-1.5.0
                                               rust-zerotrie-0.2.5
                                               rust-zerovec-0.11.7
                                               rust-zerovec-derive-0.11.4
                                               rust-zip-3.0.0
                                               rust-zlib-rs-0.6.7
                                               rust-zmij-1.0.23
                                               rust-zopfli-0.8.3
                                               rust-zstd-0.13.3
                                               rust-zstd-safe-7.2.4
                                               rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-game-core-1.38.8 =>
                                             (list rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.5
                                              rust-allocator-api2-0.2.21
                                              rust-anyhow-1.0.104
                                              rust-arbitrary-1.4.2
                                              rust-atomic-waker-1.1.2
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.13.1
                                              rust-block-buffer-0.10.4
                                              rust-bumpalo-3.20.3
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.12.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cc-1.4.2
                                              rust-cfg-if-1.0.4
                                              rust-cfg-aliases-0.2.2
                                              rust-chacha20-0.10.1
                                              rust-cipher-0.4.4
                                              rust-constant-time-eq-0.3.1
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-cpufeatures-0.3.0
                                              rust-crc-3.4.0
                                              rust-crc-catalog-2.5.0
                                              rust-crc32fast-1.5.0
                                              rust-crossbeam-channel-0.5.16
                                              rust-crossbeam-utils-0.8.22
                                              rust-crypto-common-0.1.7
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.12
                                              rust-deranged-0.5.8
                                              rust-derive-arbitrary-1.4.2
                                              rust-digest-0.10.7
                                              rust-displaydoc-0.2.7
                                              rust-dns-lookup-2.1.1
                                              rust-doctest-file-1.1.1
                                              rust-either-1.17.0
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.14
                                              rust-fastrand-2.5.0
                                              rust-filetime-0.2.29
                                              rust-find-msvc-tools-0.1.10
                                              rust-fixedbitset-0.5.7
                                              rust-flate2-1.1.9
                                              rust-fnv-1.0.7
                                              rust-foldhash-0.1.5
                                              rust-form-urlencoded-1.2.2
                                              rust-fs2-0.4.3
                                              rust-fs-extra-1.3.0
                                              rust-futures-channel-0.3.34
                                              rust-futures-core-0.3.34
                                              rust-futures-io-0.3.34
                                              rust-futures-sink-0.3.34
                                              rust-futures-task-0.3.34
                                              rust-futures-util-0.3.34
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.17
                                              rust-getrandom-0.3.4
                                              rust-getrandom-0.4.3
                                              rust-h2-0.4.15
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.5
                                              rust-hashbrown-0.17.1
                                              rust-heck-0.5.0
                                              rust-hmac-0.12.1
                                              rust-home-0.5.12
                                              rust-http-1.5.0
                                              rust-http-body-1.1.0
                                              rust-http-body-util-0.1.5
                                              rust-httparse-1.10.1
                                              rust-hyper-1.11.0
                                              rust-hyper-rustls-0.27.9
                                              rust-hyper-util-0.1.20
                                              rust-icu-collections-2.3.0
                                              rust-icu-locale-core-2.3.0
                                              rust-icu-normalizer-2.3.0
                                              rust-icu-normalizer-data-2.3.0
                                              rust-icu-properties-2.3.0
                                              rust-icu-properties-data-2.3.0
                                              rust-icu-provider-2.3.0
                                              rust-ident-case-1.0.1
                                              rust-idna-1.1.0
                                              rust-idna-adapter-1.2.2
                                              rust-indexmap-2.14.0
                                              rust-inout-0.1.4
                                              rust-interprocess-2.4.3
                                              rust-ipnet-2.12.1
                                              rust-itertools-0.14.0
                                              rust-itoa-1.0.18
                                              rust-jobserver-0.1.35
                                              rust-js-sys-0.3.104
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-libc-0.2.189
                                              rust-linux-raw-sys-0.4.15
                                              rust-linux-raw-sys-0.12.1
                                              rust-litemap-0.8.3
                                              rust-log-0.4.33
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.8.3
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.1
                                              rust-mio-1.2.2
                                              rust-multimap-0.10.1
                                              rust-ntapi-0.4.3
                                              rust-num-conv-0.2.2
                                              rust-objc2-core-foundation-0.3.2
                                              rust-objc2-io-kit-0.3.2
                                              rust-once-cell-1.21.4
                                              rust-openssl-probe-0.1.6
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.2
                                              rust-petgraph-0.8.3
                                              rust-pin-project-lite-0.2.17
                                              rust-pkg-config-0.3.34
                                              rust-potential-utf-0.1.6
                                              rust-powerfmt-0.2.0
                                              rust-prettyplease-0.2.37
                                              rust-proc-macro2-1.0.107
                                              rust-prost-0.14.4
                                              rust-prost-build-0.14.4
                                              rust-prost-derive-0.14.4
                                              rust-prost-types-0.14.4
                                              rust-protobuf-3.7.2
                                              rust-protobuf-codegen-3.7.2
                                              rust-protobuf-parse-3.7.2
                                              rust-protobuf-support-3.7.2
                                              rust-quinn-0.11.11
                                              rust-quinn-proto-0.11.16
                                              rust-quinn-udp-0.5.15
                                              rust-quote-1.0.47
                                              rust-r-efi-5.3.0
                                              rust-r-efi-6.0.0
                                              rust-rand-0.10.2
                                              rust-rand-core-0.10.1
                                              rust-rand-pcg-0.10.2
                                              rust-recvmsg-1.0.0
                                              rust-regex-1.13.1
                                              rust-regex-automata-0.4.18
                                              rust-regex-syntax-0.8.11
                                              rust-reqwest-0.12.28
                                              rust-ring-0.17.14
                                              rust-rustc-hash-2.1.3
                                              rust-rustix-0.38.44
                                              rust-rustix-1.1.4
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.43
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.15.1
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.14
                                              rust-rustversion-1.0.23
                                              rust-ryu-1.0.23
                                              rust-schannel-0.1.29
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.17.0
                                              rust-serde-1.0.229
                                              rust-serde-core-1.0.229
                                              rust-serde-derive-1.0.229
                                              rust-serde-json-1.0.151
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.7
                                              rust-shlex-2.0.1
                                              rust-simd-adler32-0.3.10
                                              rust-slab-0.4.12
                                              rust-smallvec-1.15.2
                                              rust-socket2-0.6.5
                                              rust-sophon-lib-0.1.6.89f4a70
                                              rust-stable-deref-trait-1.2.1
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.119
                                              rust-syn-3.0.3
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-tar-0.4.46
                                              rust-tempfile-3.27.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.20
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.20
                                              rust-time-0.3.55
                                              rust-time-core-0.1.9
                                              rust-tinystr-0.8.4
                                              rust-tinyvec-1.12.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.53.1
                                              rust-tokio-rustls-0.26.4
                                              rust-tokio-util-0.7.19
                                              rust-tower-0.5.3
                                              rust-tower-http-0.6.11
                                              rust-tower-layer-0.3.3
                                              rust-tower-service-0.3.3
                                              rust-tracing-0.1.44
                                              rust-tracing-attributes-0.1.31
                                              rust-tracing-core-0.1.36
                                              rust-try-lock-0.2.5
                                              rust-typenum-1.20.1
                                              rust-unicode-ident-1.0.24
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.8
                                              rust-utf8-iter-1.0.4
                                              rust-version-check-0.9.5
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasip2-1.0.4+wasi-0.2.12
                                              rust-wasm-bindgen-0.2.127
                                              rust-wasm-bindgen-futures-0.4.77
                                              rust-wasm-bindgen-macro-0.2.127
                                              rust-wasm-bindgen-macro-support-0.2.127
                                              rust-wasm-bindgen-shared-0.2.127
                                              rust-web-sys-0.3.104
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-1.0.9
                                              rust-which-4.4.2
                                              rust-widestring-1.2.1
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
                                              rust-wit-bindgen-0.57.1
                                              rust-writeable-0.6.4
                                              rust-xattr-1.6.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.3
                                              rust-yoke-derive-0.8.2
                                              rust-zerocopy-0.8.56
                                              rust-zerocopy-derive-0.8.56
                                              rust-zerofrom-0.1.8
                                              rust-zerofrom-derive-0.1.7
                                              rust-zeroize-1.9.0
                                              rust-zeroize-derive-1.5.0
                                              rust-zerotrie-0.2.5
                                              rust-zerovec-0.11.7
                                              rust-zerovec-derive-0.11.4
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.6.7
                                              rust-zmij-1.0.23
                                              rust-zopfli-0.8.3
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-game-core-1.39.1 =>
                                             (list rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.5
                                              rust-allocator-api2-0.2.21
                                              rust-anyhow-1.0.104
                                              rust-arbitrary-1.4.2
                                              rust-atomic-waker-1.1.2
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.13.1
                                              rust-block-buffer-0.10.4
                                              rust-bumpalo-3.20.3
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.12.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cc-1.4.2
                                              rust-cfg-if-1.0.4
                                              rust-cfg-aliases-0.2.2
                                              rust-chacha20-0.10.1
                                              rust-cipher-0.4.4
                                              rust-constant-time-eq-0.3.1
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-cpufeatures-0.3.0
                                              rust-crc-3.4.0
                                              rust-crc-catalog-2.5.0
                                              rust-crc32fast-1.5.0
                                              rust-crossbeam-channel-0.5.16
                                              rust-crossbeam-utils-0.8.22
                                              rust-crypto-common-0.1.7
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.12
                                              rust-deranged-0.5.8
                                              rust-derive-arbitrary-1.4.2
                                              rust-digest-0.10.7
                                              rust-displaydoc-0.2.7
                                              rust-dns-lookup-2.1.1
                                              rust-doctest-file-1.1.1
                                              rust-either-1.17.0
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.14
                                              rust-fastrand-2.5.0
                                              rust-filetime-0.2.29
                                              rust-find-msvc-tools-0.1.10
                                              rust-fixedbitset-0.5.7
                                              rust-flate2-1.1.9
                                              rust-fnv-1.0.7
                                              rust-foldhash-0.1.5
                                              rust-form-urlencoded-1.2.2
                                              rust-fs2-0.4.3
                                              rust-fs-extra-1.3.0
                                              rust-futures-channel-0.3.34
                                              rust-futures-core-0.3.34
                                              rust-futures-io-0.3.34
                                              rust-futures-sink-0.3.34
                                              rust-futures-task-0.3.34
                                              rust-futures-util-0.3.34
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.17
                                              rust-getrandom-0.3.4
                                              rust-getrandom-0.4.3
                                              rust-h2-0.4.15
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.5
                                              rust-hashbrown-0.17.1
                                              rust-heck-0.5.0
                                              rust-hmac-0.12.1
                                              rust-home-0.5.12
                                              rust-http-1.5.0
                                              rust-http-body-1.1.0
                                              rust-http-body-util-0.1.5
                                              rust-httparse-1.10.1
                                              rust-hyper-1.11.0
                                              rust-hyper-rustls-0.27.9
                                              rust-hyper-util-0.1.20
                                              rust-icu-collections-2.3.0
                                              rust-icu-locale-core-2.3.0
                                              rust-icu-normalizer-2.3.0
                                              rust-icu-normalizer-data-2.3.0
                                              rust-icu-properties-2.3.0
                                              rust-icu-properties-data-2.3.0
                                              rust-icu-provider-2.3.0
                                              rust-ident-case-1.0.1
                                              rust-idna-1.1.0
                                              rust-idna-adapter-1.2.2
                                              rust-indexmap-2.14.0
                                              rust-inout-0.1.4
                                              rust-interprocess-2.4.3
                                              rust-ipnet-2.12.1
                                              rust-itertools-0.14.0
                                              rust-itoa-1.0.18
                                              rust-jobserver-0.1.35
                                              rust-js-sys-0.3.104
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-libc-0.2.189
                                              rust-linux-raw-sys-0.4.15
                                              rust-linux-raw-sys-0.12.1
                                              rust-litemap-0.8.3
                                              rust-log-0.4.33
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.8.3
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.1
                                              rust-mio-1.2.2
                                              rust-multimap-0.10.1
                                              rust-ntapi-0.4.3
                                              rust-num-conv-0.2.2
                                              rust-objc2-core-foundation-0.3.2
                                              rust-objc2-io-kit-0.3.2
                                              rust-once-cell-1.21.4
                                              rust-openssl-probe-0.1.6
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.2
                                              rust-petgraph-0.8.3
                                              rust-pin-project-lite-0.2.17
                                              rust-pkg-config-0.3.34
                                              rust-potential-utf-0.1.6
                                              rust-powerfmt-0.2.0
                                              rust-prettyplease-0.2.37
                                              rust-proc-macro2-1.0.107
                                              rust-prost-0.14.4
                                              rust-prost-build-0.14.4
                                              rust-prost-derive-0.14.4
                                              rust-prost-types-0.14.4
                                              rust-protobuf-3.7.2
                                              rust-protobuf-codegen-3.7.2
                                              rust-protobuf-parse-3.7.2
                                              rust-protobuf-support-3.7.2
                                              rust-quinn-0.11.11
                                              rust-quinn-proto-0.11.16
                                              rust-quinn-udp-0.5.15
                                              rust-quote-1.0.47
                                              rust-r-efi-5.3.0
                                              rust-r-efi-6.0.0
                                              rust-rand-0.10.2
                                              rust-rand-core-0.10.1
                                              rust-rand-pcg-0.10.2
                                              rust-recvmsg-1.0.0
                                              rust-regex-1.13.1
                                              rust-regex-automata-0.4.18
                                              rust-regex-syntax-0.8.11
                                              rust-reqwest-0.12.28
                                              rust-ring-0.17.14
                                              rust-rustc-hash-2.1.3
                                              rust-rustix-0.38.44
                                              rust-rustix-1.1.4
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.43
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.15.1
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.14
                                              rust-rustversion-1.0.23
                                              rust-ryu-1.0.23
                                              rust-schannel-0.1.29
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.17.0
                                              rust-serde-1.0.229
                                              rust-serde-core-1.0.229
                                              rust-serde-derive-1.0.229
                                              rust-serde-json-1.0.151
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.7
                                              rust-shlex-2.0.1
                                              rust-simd-adler32-0.3.10
                                              rust-slab-0.4.12
                                              rust-smallvec-1.15.2
                                              rust-socket2-0.6.5
                                              rust-sophon-lib-0.1.8.58d223a
                                              rust-stable-deref-trait-1.2.1
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.119
                                              rust-syn-3.0.3
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-tar-0.4.46
                                              rust-tempfile-3.27.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.20
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.20
                                              rust-time-0.3.55
                                              rust-time-core-0.1.9
                                              rust-tinystr-0.8.4
                                              rust-tinyvec-1.12.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.53.1
                                              rust-tokio-rustls-0.26.4
                                              rust-tokio-util-0.7.19
                                              rust-tower-0.5.3
                                              rust-tower-http-0.6.11
                                              rust-tower-layer-0.3.3
                                              rust-tower-service-0.3.3
                                              rust-tracing-0.1.44
                                              rust-tracing-attributes-0.1.31
                                              rust-tracing-core-0.1.36
                                              rust-try-lock-0.2.5
                                              rust-typenum-1.20.1
                                              rust-unicode-ident-1.0.24
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.8
                                              rust-utf8-iter-1.0.4
                                              rust-version-check-0.9.5
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasip2-1.0.4+wasi-0.2.12
                                              rust-wasm-bindgen-0.2.127
                                              rust-wasm-bindgen-futures-0.4.77
                                              rust-wasm-bindgen-macro-0.2.127
                                              rust-wasm-bindgen-macro-support-0.2.127
                                              rust-wasm-bindgen-shared-0.2.127
                                              rust-web-sys-0.3.104
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-1.0.9
                                              rust-which-4.4.2
                                              rust-widestring-1.2.1
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
                                              rust-wit-bindgen-0.57.1
                                              rust-writeable-0.6.4
                                              rust-xattr-1.6.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.3
                                              rust-yoke-derive-0.8.2
                                              rust-zerocopy-0.8.56
                                              rust-zerocopy-derive-0.8.56
                                              rust-zerofrom-0.1.8
                                              rust-zerofrom-derive-0.1.7
                                              rust-zeroize-1.9.0
                                              rust-zeroize-derive-1.5.0
                                              rust-zerotrie-0.2.5
                                              rust-zerovec-0.11.7
                                              rust-zerovec-derive-0.11.4
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.6.7
                                              rust-zmij-1.0.23
                                              rust-zopfli-0.8.3
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-game-core-1.39.3 =>
                                             (list rust-adler2-2.0.1
                                              rust-aes-0.8.4
                                              rust-ahash-0.8.12
                                              rust-aho-corasick-1.1.5
                                              rust-allocator-api2-0.2.21
                                              rust-anyhow-1.0.104
                                              rust-arbitrary-1.4.2
                                              rust-atomic-waker-1.1.2
                                              rust-base64-0.21.7
                                              rust-base64-0.22.1
                                              rust-bitflags-2.13.1
                                              rust-block-buffer-0.10.4
                                              rust-bumpalo-3.20.3
                                              rust-byteorder-1.5.0
                                              rust-bytes-1.12.1
                                              rust-bzip2-0.4.4
                                              rust-bzip2-0.5.2
                                              rust-bzip2-sys-0.1.13+1.0.8
                                              rust-cached-0.55.1
                                              rust-cached-proc-macro-0.24.0
                                              rust-cached-proc-macro-types-0.1.1
                                              rust-cc-1.4.4
                                              rust-cfg-if-1.0.4
                                              rust-cfg-aliases-0.2.2
                                              rust-chacha20-0.10.1
                                              rust-cipher-0.4.4
                                              rust-constant-time-eq-0.3.1
                                              rust-core-foundation-0.9.4
                                              rust-core-foundation-sys-0.8.7
                                              rust-cpufeatures-0.2.17
                                              rust-cpufeatures-0.3.0
                                              rust-crc-3.4.0
                                              rust-crc-catalog-2.5.0
                                              rust-crc32fast-1.5.1
                                              rust-crossbeam-channel-0.5.16
                                              rust-crossbeam-utils-0.8.22
                                              rust-crypto-common-0.1.7
                                              rust-darling-0.20.11
                                              rust-darling-core-0.20.11
                                              rust-darling-macro-0.20.11
                                              rust-deflate64-0.1.12
                                              rust-deranged-0.5.8
                                              rust-derive-arbitrary-1.4.2
                                              rust-digest-0.10.7
                                              rust-displaydoc-0.2.7
                                              rust-dns-lookup-2.1.1
                                              rust-doctest-file-1.1.1
                                              rust-either-1.18.0
                                              rust-equivalent-1.0.2
                                              rust-errno-0.3.14
                                              rust-fastrand-2.5.0
                                              rust-filetime-0.2.29
                                              rust-find-msvc-tools-0.1.11
                                              rust-fixedbitset-0.5.7
                                              rust-flate2-1.1.9
                                              rust-fnv-1.0.7
                                              rust-foldhash-0.1.5
                                              rust-form-urlencoded-1.2.2
                                              rust-fs2-0.4.3
                                              rust-fs-extra-1.3.0
                                              rust-futures-channel-0.3.34
                                              rust-futures-core-0.3.34
                                              rust-futures-io-0.3.34
                                              rust-futures-sink-0.3.34
                                              rust-futures-task-0.3.34
                                              rust-futures-util-0.3.34
                                              rust-generic-array-0.14.7
                                              rust-getrandom-0.2.17
                                              rust-getrandom-0.3.4
                                              rust-getrandom-0.4.3
                                              rust-h2-0.4.19
                                              rust-hashbrown-0.14.5
                                              rust-hashbrown-0.15.5
                                              rust-hashbrown-0.17.1
                                              rust-heck-0.5.0
                                              rust-hmac-0.12.1
                                              rust-home-0.5.12
                                              rust-http-1.5.0
                                              rust-http-body-1.1.0
                                              rust-http-body-util-0.1.5
                                              rust-httparse-1.10.1
                                              rust-hyper-1.11.0
                                              rust-hyper-rustls-0.27.9
                                              rust-hyper-util-0.1.20
                                              rust-icu-collections-2.3.0
                                              rust-icu-locale-core-2.3.0
                                              rust-icu-normalizer-2.3.0
                                              rust-icu-normalizer-data-2.3.0
                                              rust-icu-properties-2.3.0
                                              rust-icu-properties-data-2.3.0
                                              rust-icu-provider-2.3.1
                                              rust-ident-case-1.0.1
                                              rust-idna-1.1.0
                                              rust-idna-adapter-1.2.2
                                              rust-indexmap-2.14.0
                                              rust-inout-0.1.4
                                              rust-interprocess-2.4.3
                                              rust-ipnet-2.12.1
                                              rust-itertools-0.14.0
                                              rust-itoa-1.0.18
                                              rust-jobserver-0.1.35
                                              rust-js-sys-0.3.104
                                              rust-kinda-virtual-fs-0.1.1
                                              rust-lazy-static-1.5.0
                                              rust-libc-0.2.189
                                              rust-linux-raw-sys-0.4.15
                                              rust-linux-raw-sys-0.12.1
                                              rust-litemap-0.8.3
                                              rust-log-0.4.34
                                              rust-lru-slab-0.1.2
                                              rust-lzma-rs-0.3.0
                                              rust-lzma-sys-0.1.20
                                              rust-md-5-0.10.6
                                              rust-md5-asm-0.5.2
                                              rust-memchr-2.8.3
                                              rust-miniz-oxide-0.8.9
                                              rust-minreq-2.14.1
                                              rust-mio-1.2.2
                                              rust-multimap-0.10.1
                                              rust-ntapi-0.4.3
                                              rust-num-conv-0.2.2
                                              rust-objc2-core-foundation-0.3.2
                                              rust-objc2-io-kit-0.3.2
                                              rust-once-cell-1.21.4
                                              rust-openssl-probe-0.1.6
                                              rust-pbkdf2-0.12.2
                                              rust-percent-encoding-2.3.2
                                              rust-petgraph-0.8.3
                                              rust-pin-project-lite-0.2.17
                                              rust-pkg-config-0.3.34
                                              rust-potential-utf-0.1.6
                                              rust-powerfmt-0.2.0
                                              rust-prettyplease-0.2.37
                                              rust-proc-macro2-1.0.107
                                              rust-prost-0.14.4
                                              rust-prost-build-0.14.4
                                              rust-prost-derive-0.14.4
                                              rust-prost-types-0.14.4
                                              rust-protobuf-3.7.2
                                              rust-protobuf-codegen-3.7.2
                                              rust-protobuf-parse-3.7.2
                                              rust-protobuf-support-3.7.2
                                              rust-quinn-0.11.11
                                              rust-quinn-proto-0.11.17
                                              rust-quinn-udp-0.5.15
                                              rust-quote-1.0.47
                                              rust-r-efi-5.3.0
                                              rust-r-efi-6.0.0
                                              rust-rand-0.10.2
                                              rust-rand-core-0.10.1
                                              rust-rand-pcg-0.10.2
                                              rust-recvmsg-1.0.0
                                              rust-regex-1.13.1
                                              rust-regex-automata-0.4.18
                                              rust-regex-syntax-0.8.11
                                              rust-reqwest-0.12.28
                                              rust-ring-0.17.14
                                              rust-rustc-hash-2.1.3
                                              rust-rustix-0.38.44
                                              rust-rustix-1.1.4
                                              rust-rustls-0.21.12
                                              rust-rustls-0.23.43
                                              rust-rustls-native-certs-0.6.3
                                              rust-rustls-pemfile-1.0.4
                                              rust-rustls-pki-types-1.15.1
                                              rust-rustls-webpki-0.101.7
                                              rust-rustls-webpki-0.103.15
                                              rust-rustversion-1.0.23
                                              rust-ryu-1.0.23
                                              rust-schannel-0.1.29
                                              rust-sct-0.7.1
                                              rust-security-framework-2.11.1
                                              rust-security-framework-sys-2.17.0
                                              rust-serde-1.0.229
                                              rust-serde-core-1.0.229
                                              rust-serde-derive-1.0.229
                                              rust-serde-json-1.0.151
                                              rust-serde-urlencoded-0.7.1
                                              rust-sha1-0.10.7
                                              rust-shlex-2.0.1
                                              rust-simd-adler32-0.3.10
                                              rust-slab-0.4.12
                                              rust-smallvec-1.15.2
                                              rust-socket2-0.6.5
                                              rust-sophon-lib-0.1.9.f422286
                                              rust-stable-deref-trait-1.2.1
                                              rust-strsim-0.11.1
                                              rust-subtle-2.6.1
                                              rust-syn-2.0.119
                                              rust-syn-3.0.4
                                              rust-sync-wrapper-1.0.2
                                              rust-synstructure-0.13.2
                                              rust-sysinfo-0.35.2
                                              rust-tar-0.4.46
                                              rust-tempfile-3.27.0
                                              rust-thiserror-1.0.69
                                              rust-thiserror-2.0.20
                                              rust-thiserror-impl-1.0.69
                                              rust-thiserror-impl-2.0.20
                                              rust-time-0.3.55
                                              rust-time-core-0.1.9
                                              rust-tinystr-0.8.4
                                              rust-tinyvec-1.12.0
                                              rust-tinyvec-macros-0.1.1
                                              rust-tokio-1.53.1
                                              rust-tokio-rustls-0.26.4
                                              rust-tokio-util-0.7.19
                                              rust-tower-0.5.3
                                              rust-tower-http-0.6.11
                                              rust-tower-layer-0.3.3
                                              rust-tower-service-0.3.3
                                              rust-tracing-0.1.44
                                              rust-tracing-attributes-0.1.31
                                              rust-tracing-core-0.1.36
                                              rust-try-lock-0.2.5
                                              rust-typenum-1.20.1
                                              rust-unicode-ident-1.0.24
                                              rust-untrusted-0.9.0
                                              rust-url-2.5.8
                                              rust-utf8-iter-1.0.4
                                              rust-version-check-0.9.5
                                              rust-want-0.3.1
                                              rust-wasi-0.11.1+wasi-snapshot-preview1
                                              rust-wasip2-1.0.4+wasi-0.2.12
                                              rust-wasm-bindgen-0.2.127
                                              rust-wasm-bindgen-futures-0.4.77
                                              rust-wasm-bindgen-macro-0.2.127
                                              rust-wasm-bindgen-macro-support-0.2.127
                                              rust-wasm-bindgen-shared-0.2.127
                                              rust-web-sys-0.3.104
                                              rust-web-time-1.1.0
                                              rust-webpki-roots-1.0.9
                                              rust-which-4.4.2
                                              rust-widestring-1.2.1
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
                                              rust-wit-bindgen-0.57.1
                                              rust-writeable-0.6.4
                                              rust-xattr-1.6.1
                                              rust-xz-0.1.0
                                              rust-xz2-0.1.7
                                              rust-yoke-0.8.3
                                              rust-yoke-derive-0.8.2
                                              rust-zerocopy-0.8.56
                                              rust-zerocopy-derive-0.8.56
                                              rust-zerofrom-0.1.8
                                              rust-zerofrom-derive-0.1.7
                                              rust-zeroize-1.9.0
                                              rust-zeroize-derive-1.5.0
                                              rust-zerotrie-0.2.5
                                              rust-zerovec-0.11.8
                                              rust-zerovec-derive-0.11.6
                                              rust-zip-3.0.0
                                              rust-zlib-rs-0.6.7
                                              rust-zmij-1.0.23
                                              rust-zopfli-0.8.3
                                              rust-zstd-0.13.3
                                              rust-zstd-safe-7.2.4
                                              rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk-1.32.0 =>
                                                (list rust-adler2-2.0.1
                                                 rust-aes-0.8.4
                                                 rust-ahash-0.8.12
                                                 rust-aho-corasick-1.1.4
                                                 rust-allocator-api2-0.2.21
                                                 rust-anime-game-core-1.36.3.044a1e8
                                                 rust-anyhow-1.0.102
                                                 rust-arbitrary-1.4.2
                                                 rust-arrayref-0.3.9
                                                 rust-arrayvec-0.7.6
                                                 rust-atomic-waker-1.1.2
                                                 rust-base64-0.21.7
                                                 rust-base64-0.22.1
                                                 rust-bitflags-2.11.1
                                                 rust-blake3-1.8.5
                                                 rust-block-buffer-0.10.4
                                                 rust-bumpalo-3.20.2
                                                 rust-byteorder-1.5.0
                                                 rust-bytes-1.11.1
                                                 rust-bzip2-0.4.4
                                                 rust-bzip2-0.5.2
                                                 rust-bzip2-sys-0.1.13+1.0.8
                                                 rust-cached-0.55.1
                                                 rust-cached-proc-macro-0.24.0
                                                 rust-cached-proc-macro-types-0.1.1
                                                 rust-cc-1.2.61
                                                 rust-cfg-if-1.0.4
                                                 rust-cfg-aliases-0.2.1
                                                 rust-cipher-0.4.4
                                                 rust-constant-time-eq-0.3.1
                                                 rust-constant-time-eq-0.4.2
                                                 rust-core-foundation-0.9.4
                                                 rust-core-foundation-sys-0.8.7
                                                 rust-cpufeatures-0.2.17
                                                 rust-cpufeatures-0.3.0
                                                 rust-crc-3.4.0
                                                 rust-crc-catalog-2.5.0
                                                 rust-crc32fast-1.5.0
                                                 rust-crossbeam-deque-0.8.6
                                                 rust-crossbeam-epoch-0.9.18
                                                 rust-crossbeam-utils-0.8.21
                                                 rust-crypto-common-0.1.7
                                                 rust-darling-0.20.11
                                                 rust-darling-core-0.20.11
                                                 rust-darling-macro-0.20.11
                                                 rust-deflate64-0.1.12
                                                 rust-deranged-0.5.8
                                                 rust-derive-arbitrary-1.4.2
                                                 rust-digest-0.10.7
                                                 rust-displaydoc-0.2.5
                                                 rust-dns-lookup-2.1.1
                                                 rust-either-1.15.0
                                                 rust-enum-ordinalize-4.3.2
                                                 rust-enum-ordinalize-derive-4.3.2
                                                 rust-equivalent-1.0.2
                                                 rust-errno-0.3.14
                                                 rust-fastrand-2.4.1
                                                 rust-filetime-0.2.27
                                                 rust-find-msvc-tools-0.1.9
                                                 rust-flate2-1.1.9
                                                 rust-fnv-1.0.7
                                                 rust-foldhash-0.1.5
                                                 rust-form-urlencoded-1.2.2
                                                 rust-fs-extra-1.3.0
                                                 rust-futures-channel-0.3.32
                                                 rust-futures-core-0.3.32
                                                 rust-futures-io-0.3.32
                                                 rust-futures-sink-0.3.32
                                                 rust-futures-task-0.3.32
                                                 rust-futures-util-0.3.32
                                                 rust-generic-array-0.14.7
                                                 rust-getrandom-0.2.17
                                                 rust-getrandom-0.3.4
                                                 rust-getrandom-0.4.2
                                                 rust-h2-0.4.13
                                                 rust-hashbrown-0.14.5
                                                 rust-hashbrown-0.15.5
                                                 rust-hashbrown-0.17.0
                                                 rust-heck-0.5.0
                                                 rust-hmac-0.12.1
                                                 rust-home-0.5.12
                                                 rust-http-1.4.0
                                                 rust-http-body-1.0.1
                                                 rust-http-body-util-0.1.3
                                                 rust-httparse-1.10.1
                                                 rust-hyper-1.9.0
                                                 rust-hyper-rustls-0.27.9
                                                 rust-hyper-util-0.1.20
                                                 rust-icu-collections-2.2.0
                                                 rust-icu-locale-core-2.2.0
                                                 rust-icu-normalizer-2.2.0
                                                 rust-icu-normalizer-data-2.2.0
                                                 rust-icu-properties-2.2.0
                                                 rust-icu-properties-data-2.2.0
                                                 rust-icu-provider-2.2.0
                                                 rust-id-arena-2.3.0
                                                 rust-ident-case-1.0.1
                                                 rust-idna-1.1.0
                                                 rust-idna-adapter-1.2.2
                                                 rust-indexmap-2.14.0
                                                 rust-inout-0.1.4
                                                 rust-ipnet-2.12.0
                                                 rust-iri-string-0.7.12
                                                 rust-itoa-1.0.18
                                                 rust-jobserver-0.1.34
                                                 rust-js-sys-0.3.95
                                                 rust-kinda-virtual-fs-0.1.1
                                                 rust-lazy-static-1.5.0
                                                 rust-leb128fmt-0.1.0
                                                 rust-libc-0.2.186
                                                 rust-libredox-0.1.16
                                                 rust-linux-raw-sys-0.4.15
                                                 rust-linux-raw-sys-0.12.1
                                                 rust-litemap-0.8.2
                                                 rust-log-0.4.29
                                                 rust-lru-slab-0.1.2
                                                 rust-lzma-rs-0.3.0
                                                 rust-lzma-sys-0.1.20
                                                 rust-md-5-0.10.6
                                                 rust-md5-asm-0.5.2
                                                 rust-memchr-2.8.0
                                                 rust-miniz-oxide-0.8.9
                                                 rust-minreq-2.14.1
                                                 rust-mio-1.2.0
                                                 rust-ntapi-0.4.3
                                                 rust-num-conv-0.2.1
                                                 rust-objc2-core-foundation-0.3.2
                                                 rust-objc2-io-kit-0.3.2
                                                 rust-once-cell-1.21.4
                                                 rust-openssl-probe-0.1.6
                                                 rust-pbkdf2-0.12.2
                                                 rust-percent-encoding-2.3.2
                                                 rust-pin-project-lite-0.2.17
                                                 rust-pkg-config-0.3.33
                                                 rust-plain-0.2.3
                                                 rust-potential-utf-0.1.5
                                                 rust-powerfmt-0.2.0
                                                 rust-ppv-lite86-0.2.21
                                                 rust-prettyplease-0.2.37
                                                 rust-proc-macro2-1.0.106
                                                 rust-protobuf-3.7.2
                                                 rust-protobuf-codegen-3.7.2
                                                 rust-protobuf-parse-3.7.2
                                                 rust-protobuf-support-3.7.2
                                                 rust-quinn-0.11.9
                                                 rust-quinn-proto-0.11.14
                                                 rust-quinn-udp-0.5.14
                                                 rust-quote-1.0.45
                                                 rust-r-efi-5.3.0
                                                 rust-r-efi-6.0.0
                                                 rust-rand-0.9.4
                                                 rust-rand-chacha-0.9.0
                                                 rust-rand-core-0.9.5
                                                 rust-redox-syscall-0.7.4
                                                 rust-regex-1.12.3
                                                 rust-regex-automata-0.4.14
                                                 rust-regex-syntax-0.8.10
                                                 rust-reqwest-0.12.28
                                                 rust-ring-0.17.14
                                                 rust-rustc-hash-2.1.2
                                                 rust-rustix-0.38.44
                                                 rust-rustix-1.1.4
                                                 rust-rustls-0.21.12
                                                 rust-rustls-0.23.39
                                                 rust-rustls-native-certs-0.6.3
                                                 rust-rustls-pemfile-1.0.4
                                                 rust-rustls-pki-types-1.14.1
                                                 rust-rustls-webpki-0.101.7
                                                 rust-rustls-webpki-0.103.13
                                                 rust-rustversion-1.0.22
                                                 rust-ryu-1.0.23
                                                 rust-schannel-0.1.29
                                                 rust-sct-0.7.1
                                                 rust-security-framework-2.11.1
                                                 rust-security-framework-sys-2.17.0
                                                 rust-semver-1.0.28
                                                 rust-serde-1.0.228
                                                 rust-serde-core-1.0.228
                                                 rust-serde-derive-1.0.228
                                                 rust-serde-json-1.0.149
                                                 rust-serde-urlencoded-0.7.1
                                                 rust-sha1-0.10.6
                                                 rust-shlex-1.3.0
                                                 rust-simd-adler32-0.3.9
                                                 rust-slab-0.4.12
                                                 rust-smallvec-1.15.1
                                                 rust-socket2-0.6.3
                                                 rust-stable-deref-trait-1.2.1
                                                 rust-strsim-0.11.1
                                                 rust-subtle-2.6.1
                                                 rust-syn-2.0.117
                                                 rust-sync-wrapper-1.0.2
                                                 rust-synstructure-0.13.2
                                                 rust-sysinfo-0.35.2
                                                 rust-tar-0.4.45
                                                 rust-tempfile-3.27.0
                                                 rust-thiserror-1.0.69
                                                 rust-thiserror-2.0.18
                                                 rust-thiserror-impl-1.0.69
                                                 rust-thiserror-impl-2.0.18
                                                 rust-time-0.3.47
                                                 rust-time-core-0.1.8
                                                 rust-tinystr-0.8.3
                                                 rust-tinyvec-1.11.0
                                                 rust-tinyvec-macros-0.1.1
                                                 rust-tokio-1.52.1
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
                                                 rust-typenum-1.20.0
                                                 rust-unicode-ident-1.0.24
                                                 rust-unicode-xid-0.2.6
                                                 rust-untrusted-0.9.0
                                                 rust-url-2.5.8
                                                 rust-utf8-iter-1.0.4
                                                 rust-version-check-0.9.5
                                                 rust-want-0.3.1
                                                 rust-wasi-0.11.1+wasi-snapshot-preview1
                                                 rust-wasip2-1.0.3+wasi-0.2.9
                                                 rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
                                                 rust-wasm-bindgen-0.2.118
                                                 rust-wasm-bindgen-futures-0.4.68
                                                 rust-wasm-bindgen-macro-0.2.118
                                                 rust-wasm-bindgen-macro-support-0.2.118
                                                 rust-wasm-bindgen-shared-0.2.118
                                                 rust-wasm-encoder-0.244.0
                                                 rust-wasm-metadata-0.244.0
                                                 rust-wasmparser-0.244.0
                                                 rust-web-sys-0.3.95
                                                 rust-web-time-1.1.0
                                                 rust-webpki-roots-0.25.4
                                                 rust-webpki-roots-1.0.7
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
                                                 rust-wit-bindgen-0.57.1
                                                 rust-wit-bindgen-core-0.51.0
                                                 rust-wit-bindgen-rust-0.51.0
                                                 rust-wit-bindgen-rust-macro-0.51.0
                                                 rust-wit-component-0.244.0
                                                 rust-wit-parser-0.244.0
                                                 rust-writeable-0.6.3
                                                 rust-xattr-1.6.1
                                                 rust-xz-0.1.0
                                                 rust-xz2-0.1.7
                                                 rust-yoke-0.8.2
                                                 rust-yoke-derive-0.8.2
                                                 rust-zerocopy-0.8.48
                                                 rust-zerocopy-derive-0.8.48
                                                 rust-zerofrom-0.1.7
                                                 rust-zerofrom-derive-0.1.7
                                                 rust-zeroize-1.8.2
                                                 rust-zeroize-derive-1.4.3
                                                 rust-zerotrie-0.2.4
                                                 rust-zerovec-0.11.6
                                                 rust-zerovec-derive-0.11.3
                                                 rust-zip-3.0.0
                                                 rust-zlib-rs-0.6.3
                                                 rust-zmij-1.0.21
                                                 rust-zopfli-0.8.3
                                                 rust-zstd-0.13.3
                                                 rust-zstd-safe-7.2.4
                                                 rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk-1.35.10 =>
                                                 (list rust-adler2-2.0.1
                                                  rust-aes-0.8.4
                                                  rust-ahash-0.8.12
                                                  rust-aho-corasick-1.1.5
                                                  rust-allocator-api2-0.2.21
                                                  rust-anime-game-core-1.38.8.de96f35
                                                  rust-anyhow-1.0.104
                                                  rust-arbitrary-1.4.2
                                                  rust-arrayref-0.3.9
                                                  rust-arrayvec-0.7.8
                                                  rust-atomic-waker-1.1.2
                                                  rust-base64-0.21.7
                                                  rust-base64-0.22.1
                                                  rust-bitflags-2.13.1
                                                  rust-blake3-1.8.6
                                                  rust-block-buffer-0.10.4
                                                  rust-bumpalo-3.20.3
                                                  rust-byteorder-1.5.0
                                                  rust-bytes-1.12.1
                                                  rust-bzip2-0.4.4
                                                  rust-bzip2-0.5.2
                                                  rust-bzip2-sys-0.1.13+1.0.8
                                                  rust-cached-0.55.1
                                                  rust-cached-proc-macro-0.24.0
                                                  rust-cached-proc-macro-types-0.1.1
                                                  rust-cc-1.4.2
                                                  rust-cfg-if-1.0.4
                                                  rust-cfg-aliases-0.2.2
                                                  rust-chacha20-0.10.1
                                                  rust-cipher-0.4.4
                                                  rust-constant-time-eq-0.3.1
                                                  rust-constant-time-eq-0.4.2
                                                  rust-core-foundation-0.9.4
                                                  rust-core-foundation-sys-0.8.7
                                                  rust-cpufeatures-0.2.17
                                                  rust-cpufeatures-0.3.0
                                                  rust-crc-3.4.0
                                                  rust-crc-catalog-2.5.0
                                                  rust-crc32fast-1.5.0
                                                  rust-crossbeam-channel-0.5.16
                                                  rust-crossbeam-utils-0.8.22
                                                  rust-crypto-common-0.1.7
                                                  rust-darling-0.20.11
                                                  rust-darling-core-0.20.11
                                                  rust-darling-macro-0.20.11
                                                  rust-deflate64-0.1.12
                                                  rust-deranged-0.5.8
                                                  rust-derive-arbitrary-1.4.2
                                                  rust-digest-0.10.7
                                                  rust-displaydoc-0.2.7
                                                  rust-dns-lookup-2.1.1
                                                  rust-doctest-file-1.1.1
                                                  rust-either-1.17.0
                                                  rust-enum-ordinalize-4.4.2
                                                  rust-enum-ordinalize-derive-4.4.2
                                                  rust-equivalent-1.0.2
                                                  rust-errno-0.3.14
                                                  rust-fastrand-2.5.0
                                                  rust-filetime-0.2.29
                                                  rust-find-msvc-tools-0.1.10
                                                  rust-fixedbitset-0.5.7
                                                  rust-flate2-1.1.9
                                                  rust-fnv-1.0.7
                                                  rust-foldhash-0.1.5
                                                  rust-form-urlencoded-1.2.2
                                                  rust-fs2-0.4.3
                                                  rust-fs-extra-1.3.0
                                                  rust-futures-channel-0.3.34
                                                  rust-futures-core-0.3.34
                                                  rust-futures-io-0.3.34
                                                  rust-futures-sink-0.3.34
                                                  rust-futures-task-0.3.34
                                                  rust-futures-util-0.3.34
                                                  rust-generic-array-0.14.7
                                                  rust-getrandom-0.2.17
                                                  rust-getrandom-0.3.4
                                                  rust-getrandom-0.4.3
                                                  rust-h2-0.4.15
                                                  rust-hashbrown-0.14.5
                                                  rust-hashbrown-0.15.5
                                                  rust-hashbrown-0.17.1
                                                  rust-heck-0.5.0
                                                  rust-hmac-0.12.1
                                                  rust-http-1.5.0
                                                  rust-http-body-1.1.0
                                                  rust-http-body-util-0.1.5
                                                  rust-httparse-1.10.1
                                                  rust-hyper-1.11.0
                                                  rust-hyper-rustls-0.27.9
                                                  rust-hyper-util-0.1.20
                                                  rust-icu-collections-2.3.0
                                                  rust-icu-locale-core-2.3.0
                                                  rust-icu-normalizer-2.3.0
                                                  rust-icu-normalizer-data-2.3.0
                                                  rust-icu-properties-2.3.0
                                                  rust-icu-properties-data-2.3.0
                                                  rust-icu-provider-2.3.0
                                                  rust-ident-case-1.0.1
                                                  rust-idna-1.1.0
                                                  rust-idna-adapter-1.2.2
                                                  rust-indexmap-2.14.0
                                                  rust-inout-0.1.4
                                                  rust-interprocess-2.4.3
                                                  rust-ipnet-2.12.1
                                                  rust-itertools-0.14.0
                                                  rust-itoa-1.0.18
                                                  rust-jobserver-0.1.35
                                                  rust-js-sys-0.3.104
                                                  rust-kinda-virtual-fs-0.1.1
                                                  rust-lazy-static-1.5.0
                                                  rust-libc-0.2.189
                                                  rust-linux-raw-sys-0.12.1
                                                  rust-litemap-0.8.3
                                                  rust-log-0.4.33
                                                  rust-lru-slab-0.1.2
                                                  rust-lzma-rs-0.3.0
                                                  rust-lzma-sys-0.1.20
                                                  rust-md-5-0.10.6
                                                  rust-md5-asm-0.5.2
                                                  rust-memchr-2.8.3
                                                  rust-miniz-oxide-0.8.9
                                                  rust-minreq-2.14.1
                                                  rust-mio-1.2.2
                                                  rust-multimap-0.10.1
                                                  rust-ntapi-0.4.3
                                                  rust-num-conv-0.2.2
                                                  rust-objc2-core-foundation-0.3.2
                                                  rust-objc2-io-kit-0.3.2
                                                  rust-once-cell-1.21.4
                                                  rust-openssl-probe-0.1.6
                                                  rust-pbkdf2-0.12.2
                                                  rust-percent-encoding-2.3.2
                                                  rust-petgraph-0.8.3
                                                  rust-pin-project-lite-0.2.17
                                                  rust-pkg-config-0.3.34
                                                  rust-potential-utf-0.1.6
                                                  rust-powerfmt-0.2.0
                                                  rust-prettyplease-0.2.37
                                                  rust-proc-macro2-1.0.107
                                                  rust-prost-0.14.4
                                                  rust-prost-build-0.14.4
                                                  rust-prost-derive-0.14.4
                                                  rust-prost-types-0.14.4
                                                  rust-quinn-0.11.11
                                                  rust-quinn-proto-0.11.16
                                                  rust-quinn-udp-0.5.15
                                                  rust-quote-1.0.47
                                                  rust-r-efi-5.3.0
                                                  rust-r-efi-6.0.0
                                                  rust-rand-0.10.2
                                                  rust-rand-core-0.10.1
                                                  rust-rand-pcg-0.10.2
                                                  rust-recvmsg-1.0.0
                                                  rust-regex-1.13.1
                                                  rust-regex-automata-0.4.18
                                                  rust-regex-syntax-0.8.11
                                                  rust-reqwest-0.12.28
                                                  rust-ring-0.17.14
                                                  rust-rustc-hash-2.1.3
                                                  rust-rustix-1.1.4
                                                  rust-rustls-0.21.12
                                                  rust-rustls-0.23.43
                                                  rust-rustls-native-certs-0.6.3
                                                  rust-rustls-pemfile-1.0.4
                                                  rust-rustls-pki-types-1.15.1
                                                  rust-rustls-webpki-0.101.7
                                                  rust-rustls-webpki-0.103.14
                                                  rust-rustversion-1.0.23
                                                  rust-ryu-1.0.23
                                                  rust-schannel-0.1.29
                                                  rust-sct-0.7.1
                                                  rust-security-framework-2.11.1
                                                  rust-security-framework-sys-2.17.0
                                                  rust-serde-1.0.229
                                                  rust-serde-core-1.0.229
                                                  rust-serde-derive-1.0.229
                                                  rust-serde-json-1.0.151
                                                  rust-serde-urlencoded-0.7.1
                                                  rust-sha1-0.10.7
                                                  rust-shlex-2.0.1
                                                  rust-simd-adler32-0.3.10
                                                  rust-slab-0.4.12
                                                  rust-smallvec-1.15.2
                                                  rust-socket2-0.6.5
                                                  rust-sophon-lib-0.1.6.89f4a70
                                                  rust-stable-deref-trait-1.2.1
                                                  rust-strsim-0.11.1
                                                  rust-subtle-2.6.1
                                                  rust-syn-2.0.119
                                                  rust-syn-3.0.3
                                                  rust-sync-wrapper-1.0.2
                                                  rust-synstructure-0.13.2
                                                  rust-sysinfo-0.35.2
                                                  rust-tar-0.4.46
                                                  rust-tempfile-3.27.0
                                                  rust-thiserror-1.0.69
                                                  rust-thiserror-2.0.20
                                                  rust-thiserror-impl-1.0.69
                                                  rust-thiserror-impl-2.0.20
                                                  rust-time-0.3.55
                                                  rust-time-core-0.1.9
                                                  rust-tinystr-0.8.4
                                                  rust-tinyvec-1.12.0
                                                  rust-tinyvec-macros-0.1.1
                                                  rust-tokio-1.53.1
                                                  rust-tokio-rustls-0.26.4
                                                  rust-tokio-util-0.7.19
                                                  rust-tower-0.5.3
                                                  rust-tower-http-0.6.11
                                                  rust-tower-layer-0.3.3
                                                  rust-tower-service-0.3.3
                                                  rust-tracing-0.1.44
                                                  rust-tracing-attributes-0.1.31
                                                  rust-tracing-core-0.1.36
                                                  rust-try-lock-0.2.5
                                                  rust-typenum-1.20.1
                                                  rust-unicode-ident-1.0.24
                                                  rust-untrusted-0.9.0
                                                  rust-url-2.5.8
                                                  rust-utf8-iter-1.0.4
                                                  rust-version-check-0.9.5
                                                  rust-want-0.3.1
                                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                                  rust-wasip2-1.0.4+wasi-0.2.12
                                                  rust-wasm-bindgen-0.2.127
                                                  rust-wasm-bindgen-futures-0.4.77
                                                  rust-wasm-bindgen-macro-0.2.127
                                                  rust-wasm-bindgen-macro-support-0.2.127
                                                  rust-wasm-bindgen-shared-0.2.127
                                                  rust-web-sys-0.3.104
                                                  rust-web-time-1.1.0
                                                  rust-webpki-roots-0.25.4
                                                  rust-webpki-roots-1.0.9
                                                  rust-widestring-1.2.1
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
                                                  rust-wit-bindgen-0.57.1
                                                  rust-writeable-0.6.4
                                                  rust-xattr-1.6.1
                                                  rust-xz-0.1.0
                                                  rust-xz2-0.1.7
                                                  rust-yoke-0.8.3
                                                  rust-yoke-derive-0.8.2
                                                  rust-zerocopy-0.8.56
                                                  rust-zerocopy-derive-0.8.56
                                                  rust-zerofrom-0.1.8
                                                  rust-zerofrom-derive-0.1.7
                                                  rust-zeroize-1.9.0
                                                  rust-zeroize-derive-1.5.0
                                                  rust-zerotrie-0.2.5
                                                  rust-zerovec-0.11.7
                                                  rust-zerovec-derive-0.11.4
                                                  rust-zip-3.0.0
                                                  rust-zlib-rs-0.6.7
                                                  rust-zmij-1.0.23
                                                  rust-zopfli-0.8.3
                                                  rust-zstd-0.13.3
                                                  rust-zstd-safe-7.2.4
                                                  rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk-1.35.12 =>
                                                 (list rust-adler2-2.0.1
                                                  rust-aes-0.8.4
                                                  rust-ahash-0.8.12
                                                  rust-aho-corasick-1.1.5
                                                  rust-allocator-api2-0.2.21
                                                  rust-anime-game-core-1.38.10.ba60faf
                                                  rust-anyhow-1.0.104
                                                  rust-arbitrary-1.4.2
                                                  rust-arrayref-0.3.9
                                                  rust-arrayvec-0.7.8
                                                  rust-atomic-waker-1.1.2
                                                  rust-base64-0.21.7
                                                  rust-base64-0.22.1
                                                  rust-bitflags-2.13.1
                                                  rust-blake3-1.8.6
                                                  rust-block-buffer-0.10.4
                                                  rust-bumpalo-3.20.3
                                                  rust-byteorder-1.5.0
                                                  rust-bytes-1.12.1
                                                  rust-bzip2-0.4.4
                                                  rust-bzip2-0.5.2
                                                  rust-bzip2-sys-0.1.13+1.0.8
                                                  rust-cached-0.55.1
                                                  rust-cached-proc-macro-0.24.0
                                                  rust-cached-proc-macro-types-0.1.1
                                                  rust-cc-1.4.2
                                                  rust-cfg-if-1.0.4
                                                  rust-cfg-aliases-0.2.2
                                                  rust-chacha20-0.10.1
                                                  rust-cipher-0.4.4
                                                  rust-constant-time-eq-0.3.1
                                                  rust-constant-time-eq-0.4.2
                                                  rust-core-foundation-0.9.4
                                                  rust-core-foundation-sys-0.8.7
                                                  rust-cpufeatures-0.2.17
                                                  rust-cpufeatures-0.3.0
                                                  rust-crc-3.4.0
                                                  rust-crc-catalog-2.5.0
                                                  rust-crc32fast-1.5.0
                                                  rust-crossbeam-channel-0.5.16
                                                  rust-crossbeam-utils-0.8.22
                                                  rust-crypto-common-0.1.7
                                                  rust-darling-0.20.11
                                                  rust-darling-core-0.20.11
                                                  rust-darling-macro-0.20.11
                                                  rust-deflate64-0.1.12
                                                  rust-deranged-0.5.8
                                                  rust-derive-arbitrary-1.4.2
                                                  rust-digest-0.10.7
                                                  rust-displaydoc-0.2.7
                                                  rust-dns-lookup-2.1.1
                                                  rust-doctest-file-1.1.1
                                                  rust-either-1.17.0
                                                  rust-enum-ordinalize-4.4.2
                                                  rust-enum-ordinalize-derive-4.4.2
                                                  rust-equivalent-1.0.2
                                                  rust-errno-0.3.14
                                                  rust-fastrand-2.5.0
                                                  rust-filetime-0.2.29
                                                  rust-find-msvc-tools-0.1.10
                                                  rust-fixedbitset-0.5.7
                                                  rust-flate2-1.1.9
                                                  rust-fnv-1.0.7
                                                  rust-foldhash-0.1.5
                                                  rust-form-urlencoded-1.2.2
                                                  rust-fs2-0.4.3
                                                  rust-fs-extra-1.3.0
                                                  rust-futures-channel-0.3.34
                                                  rust-futures-core-0.3.34
                                                  rust-futures-io-0.3.34
                                                  rust-futures-sink-0.3.34
                                                  rust-futures-task-0.3.34
                                                  rust-futures-util-0.3.34
                                                  rust-generic-array-0.14.7
                                                  rust-getrandom-0.2.17
                                                  rust-getrandom-0.3.4
                                                  rust-getrandom-0.4.3
                                                  rust-h2-0.4.15
                                                  rust-hashbrown-0.14.5
                                                  rust-hashbrown-0.15.5
                                                  rust-hashbrown-0.17.1
                                                  rust-heck-0.5.0
                                                  rust-hmac-0.12.1
                                                  rust-http-1.5.0
                                                  rust-http-body-1.1.0
                                                  rust-http-body-util-0.1.5
                                                  rust-httparse-1.10.1
                                                  rust-hyper-1.11.0
                                                  rust-hyper-rustls-0.27.9
                                                  rust-hyper-util-0.1.20
                                                  rust-icu-collections-2.3.0
                                                  rust-icu-locale-core-2.3.0
                                                  rust-icu-normalizer-2.3.0
                                                  rust-icu-normalizer-data-2.3.0
                                                  rust-icu-properties-2.3.0
                                                  rust-icu-properties-data-2.3.0
                                                  rust-icu-provider-2.3.0
                                                  rust-ident-case-1.0.1
                                                  rust-idna-1.1.0
                                                  rust-idna-adapter-1.2.2
                                                  rust-indexmap-2.14.0
                                                  rust-inout-0.1.4
                                                  rust-interprocess-2.4.3
                                                  rust-ipnet-2.12.1
                                                  rust-itertools-0.14.0
                                                  rust-itoa-1.0.18
                                                  rust-jobserver-0.1.35
                                                  rust-js-sys-0.3.104
                                                  rust-kinda-virtual-fs-0.1.1
                                                  rust-lazy-static-1.5.0
                                                  rust-libc-0.2.189
                                                  rust-linux-raw-sys-0.12.1
                                                  rust-litemap-0.8.3
                                                  rust-log-0.4.33
                                                  rust-lru-slab-0.1.2
                                                  rust-lzma-rs-0.3.0
                                                  rust-lzma-sys-0.1.20
                                                  rust-md-5-0.10.6
                                                  rust-md5-asm-0.5.2
                                                  rust-memchr-2.8.3
                                                  rust-miniz-oxide-0.8.9
                                                  rust-minreq-2.14.1
                                                  rust-mio-1.2.2
                                                  rust-multimap-0.10.1
                                                  rust-ntapi-0.4.3
                                                  rust-num-conv-0.2.2
                                                  rust-objc2-core-foundation-0.3.2
                                                  rust-objc2-io-kit-0.3.2
                                                  rust-once-cell-1.21.4
                                                  rust-openssl-probe-0.1.6
                                                  rust-pbkdf2-0.12.2
                                                  rust-percent-encoding-2.3.2
                                                  rust-petgraph-0.8.3
                                                  rust-pin-project-lite-0.2.17
                                                  rust-pkg-config-0.3.34
                                                  rust-potential-utf-0.1.6
                                                  rust-powerfmt-0.2.0
                                                  rust-prettyplease-0.2.37
                                                  rust-proc-macro2-1.0.107
                                                  rust-prost-0.14.4
                                                  rust-prost-build-0.14.4
                                                  rust-prost-derive-0.14.4
                                                  rust-prost-types-0.14.4
                                                  rust-quinn-0.11.11
                                                  rust-quinn-proto-0.11.16
                                                  rust-quinn-udp-0.5.15
                                                  rust-quote-1.0.47
                                                  rust-r-efi-5.3.0
                                                  rust-r-efi-6.0.0
                                                  rust-rand-0.10.2
                                                  rust-rand-core-0.10.1
                                                  rust-rand-pcg-0.10.2
                                                  rust-recvmsg-1.0.0
                                                  rust-regex-1.13.1
                                                  rust-regex-automata-0.4.18
                                                  rust-regex-syntax-0.8.11
                                                  rust-reqwest-0.12.28
                                                  rust-ring-0.17.14
                                                  rust-rustc-hash-2.1.3
                                                  rust-rustix-1.1.4
                                                  rust-rustls-0.21.12
                                                  rust-rustls-0.23.43
                                                  rust-rustls-native-certs-0.6.3
                                                  rust-rustls-pemfile-1.0.4
                                                  rust-rustls-pki-types-1.15.1
                                                  rust-rustls-webpki-0.101.7
                                                  rust-rustls-webpki-0.103.14
                                                  rust-rustversion-1.0.23
                                                  rust-ryu-1.0.23
                                                  rust-schannel-0.1.29
                                                  rust-sct-0.7.1
                                                  rust-security-framework-2.11.1
                                                  rust-security-framework-sys-2.17.0
                                                  rust-serde-1.0.229
                                                  rust-serde-core-1.0.229
                                                  rust-serde-derive-1.0.229
                                                  rust-serde-json-1.0.151
                                                  rust-serde-urlencoded-0.7.1
                                                  rust-sha1-0.10.7
                                                  rust-shlex-2.0.1
                                                  rust-simd-adler32-0.3.10
                                                  rust-slab-0.4.12
                                                  rust-smallvec-1.15.2
                                                  rust-socket2-0.6.5
                                                  rust-sophon-lib-0.1.8.58d223a
                                                  rust-stable-deref-trait-1.2.1
                                                  rust-strsim-0.11.1
                                                  rust-subtle-2.6.1
                                                  rust-syn-2.0.119
                                                  rust-syn-3.0.3
                                                  rust-sync-wrapper-1.0.2
                                                  rust-synstructure-0.13.2
                                                  rust-sysinfo-0.35.2
                                                  rust-tar-0.4.46
                                                  rust-tempfile-3.27.0
                                                  rust-thiserror-1.0.69
                                                  rust-thiserror-2.0.20
                                                  rust-thiserror-impl-1.0.69
                                                  rust-thiserror-impl-2.0.20
                                                  rust-time-0.3.55
                                                  rust-time-core-0.1.9
                                                  rust-tinystr-0.8.4
                                                  rust-tinyvec-1.12.0
                                                  rust-tinyvec-macros-0.1.1
                                                  rust-tokio-1.53.1
                                                  rust-tokio-rustls-0.26.4
                                                  rust-tokio-util-0.7.19
                                                  rust-tower-0.5.3
                                                  rust-tower-http-0.6.11
                                                  rust-tower-layer-0.3.3
                                                  rust-tower-service-0.3.3
                                                  rust-tracing-0.1.44
                                                  rust-tracing-attributes-0.1.31
                                                  rust-tracing-core-0.1.36
                                                  rust-try-lock-0.2.5
                                                  rust-typenum-1.20.1
                                                  rust-unicode-ident-1.0.24
                                                  rust-untrusted-0.9.0
                                                  rust-url-2.5.8
                                                  rust-utf8-iter-1.0.4
                                                  rust-version-check-0.9.5
                                                  rust-want-0.3.1
                                                  rust-wasi-0.11.1+wasi-snapshot-preview1
                                                  rust-wasip2-1.0.4+wasi-0.2.12
                                                  rust-wasm-bindgen-0.2.127
                                                  rust-wasm-bindgen-futures-0.4.77
                                                  rust-wasm-bindgen-macro-0.2.127
                                                  rust-wasm-bindgen-macro-support-0.2.127
                                                  rust-wasm-bindgen-shared-0.2.127
                                                  rust-web-sys-0.3.104
                                                  rust-web-time-1.1.0
                                                  rust-webpki-roots-0.25.4
                                                  rust-webpki-roots-1.0.9
                                                  rust-widestring-1.2.1
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
                                                  rust-wit-bindgen-0.57.1
                                                  rust-writeable-0.6.4
                                                  rust-xattr-1.6.1
                                                  rust-xz-0.1.0
                                                  rust-xz2-0.1.7
                                                  rust-yoke-0.8.3
                                                  rust-yoke-derive-0.8.2
                                                  rust-zerocopy-0.8.56
                                                  rust-zerocopy-derive-0.8.56
                                                  rust-zerofrom-0.1.8
                                                  rust-zerofrom-derive-0.1.7
                                                  rust-zeroize-1.9.0
                                                  rust-zeroize-derive-1.5.0
                                                  rust-zerotrie-0.2.5
                                                  rust-zerovec-0.11.7
                                                  rust-zerovec-derive-0.11.4
                                                  rust-zip-3.0.0
                                                  rust-zlib-rs-0.6.7
                                                  rust-zmij-1.0.23
                                                  rust-zopfli-0.8.3
                                                  rust-zstd-0.13.3
                                                  rust-zstd-safe-7.2.4
                                                  rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk-1.36.4 =>
                                                (list rust-adler2-2.0.1
                                                 rust-aes-0.8.4
                                                 rust-ahash-0.8.12
                                                 rust-aho-corasick-1.1.5
                                                 rust-allocator-api2-0.2.21
                                                 rust-anime-game-core-1.39.1.aa8c5ce
                                                 rust-anyhow-1.0.104
                                                 rust-arbitrary-1.4.2
                                                 rust-arrayref-0.3.9
                                                 rust-arrayvec-0.7.8
                                                 rust-atomic-waker-1.1.2
                                                 rust-base64-0.21.7
                                                 rust-base64-0.22.1
                                                 rust-bitflags-2.13.1
                                                 rust-blake3-1.8.6
                                                 rust-block-buffer-0.10.4
                                                 rust-bumpalo-3.20.3
                                                 rust-byteorder-1.5.0
                                                 rust-bytes-1.12.1
                                                 rust-bzip2-0.4.4
                                                 rust-bzip2-0.5.2
                                                 rust-bzip2-sys-0.1.13+1.0.8
                                                 rust-cached-0.55.1
                                                 rust-cached-proc-macro-0.24.0
                                                 rust-cached-proc-macro-types-0.1.1
                                                 rust-cc-1.4.2
                                                 rust-cfg-if-1.0.4
                                                 rust-cfg-aliases-0.2.2
                                                 rust-chacha20-0.10.1
                                                 rust-cipher-0.4.4
                                                 rust-constant-time-eq-0.3.1
                                                 rust-constant-time-eq-0.4.2
                                                 rust-core-foundation-0.9.4
                                                 rust-core-foundation-sys-0.8.7
                                                 rust-cpufeatures-0.2.17
                                                 rust-cpufeatures-0.3.0
                                                 rust-crc-3.4.0
                                                 rust-crc-catalog-2.5.0
                                                 rust-crc32fast-1.5.0
                                                 rust-crossbeam-channel-0.5.16
                                                 rust-crossbeam-utils-0.8.22
                                                 rust-crypto-common-0.1.7
                                                 rust-darling-0.20.11
                                                 rust-darling-core-0.20.11
                                                 rust-darling-macro-0.20.11
                                                 rust-deflate64-0.1.12
                                                 rust-deranged-0.5.8
                                                 rust-derive-arbitrary-1.4.2
                                                 rust-digest-0.10.7
                                                 rust-displaydoc-0.2.7
                                                 rust-dns-lookup-2.1.1
                                                 rust-doctest-file-1.1.1
                                                 rust-either-1.17.0
                                                 rust-enum-ordinalize-4.4.2
                                                 rust-enum-ordinalize-derive-4.4.2
                                                 rust-equivalent-1.0.2
                                                 rust-errno-0.3.14
                                                 rust-fastrand-2.5.0
                                                 rust-filetime-0.2.29
                                                 rust-find-msvc-tools-0.1.10
                                                 rust-fixedbitset-0.5.7
                                                 rust-flate2-1.1.9
                                                 rust-fnv-1.0.7
                                                 rust-foldhash-0.1.5
                                                 rust-form-urlencoded-1.2.2
                                                 rust-fs2-0.4.3
                                                 rust-fs-extra-1.3.0
                                                 rust-futures-channel-0.3.34
                                                 rust-futures-core-0.3.34
                                                 rust-futures-io-0.3.34
                                                 rust-futures-sink-0.3.34
                                                 rust-futures-task-0.3.34
                                                 rust-futures-util-0.3.34
                                                 rust-generic-array-0.14.7
                                                 rust-getrandom-0.2.17
                                                 rust-getrandom-0.3.4
                                                 rust-getrandom-0.4.3
                                                 rust-h2-0.4.15
                                                 rust-hashbrown-0.14.5
                                                 rust-hashbrown-0.15.5
                                                 rust-hashbrown-0.17.1
                                                 rust-heck-0.5.0
                                                 rust-hmac-0.12.1
                                                 rust-http-1.5.0
                                                 rust-http-body-1.1.0
                                                 rust-http-body-util-0.1.5
                                                 rust-httparse-1.10.1
                                                 rust-hyper-1.11.0
                                                 rust-hyper-rustls-0.27.9
                                                 rust-hyper-util-0.1.20
                                                 rust-icu-collections-2.3.0
                                                 rust-icu-locale-core-2.3.0
                                                 rust-icu-normalizer-2.3.0
                                                 rust-icu-normalizer-data-2.3.0
                                                 rust-icu-properties-2.3.0
                                                 rust-icu-properties-data-2.3.0
                                                 rust-icu-provider-2.3.0
                                                 rust-ident-case-1.0.1
                                                 rust-idna-1.1.0
                                                 rust-idna-adapter-1.2.2
                                                 rust-indexmap-2.14.0
                                                 rust-inout-0.1.4
                                                 rust-interprocess-2.4.3
                                                 rust-ipnet-2.12.1
                                                 rust-itertools-0.14.0
                                                 rust-itoa-1.0.18
                                                 rust-jobserver-0.1.35
                                                 rust-js-sys-0.3.104
                                                 rust-kinda-virtual-fs-0.1.1
                                                 rust-lazy-static-1.5.0
                                                 rust-libc-0.2.189
                                                 rust-linux-raw-sys-0.12.1
                                                 rust-litemap-0.8.3
                                                 rust-log-0.4.33
                                                 rust-lru-slab-0.1.2
                                                 rust-lzma-rs-0.3.0
                                                 rust-lzma-sys-0.1.20
                                                 rust-md-5-0.10.6
                                                 rust-md5-asm-0.5.2
                                                 rust-memchr-2.8.3
                                                 rust-miniz-oxide-0.8.9
                                                 rust-minreq-2.14.1
                                                 rust-mio-1.2.2
                                                 rust-multimap-0.10.1
                                                 rust-ntapi-0.4.3
                                                 rust-num-conv-0.2.2
                                                 rust-objc2-core-foundation-0.3.2
                                                 rust-objc2-io-kit-0.3.2
                                                 rust-once-cell-1.21.4
                                                 rust-openssl-probe-0.1.6
                                                 rust-pbkdf2-0.12.2
                                                 rust-percent-encoding-2.3.2
                                                 rust-petgraph-0.8.3
                                                 rust-pin-project-lite-0.2.17
                                                 rust-pkg-config-0.3.34
                                                 rust-potential-utf-0.1.6
                                                 rust-powerfmt-0.2.0
                                                 rust-prettyplease-0.2.37
                                                 rust-proc-macro2-1.0.107
                                                 rust-prost-0.14.4
                                                 rust-prost-build-0.14.4
                                                 rust-prost-derive-0.14.4
                                                 rust-prost-types-0.14.4
                                                 rust-quinn-0.11.11
                                                 rust-quinn-proto-0.11.16
                                                 rust-quinn-udp-0.5.15
                                                 rust-quote-1.0.47
                                                 rust-r-efi-5.3.0
                                                 rust-r-efi-6.0.0
                                                 rust-rand-0.10.2
                                                 rust-rand-core-0.10.1
                                                 rust-rand-pcg-0.10.2
                                                 rust-recvmsg-1.0.0
                                                 rust-regex-1.13.1
                                                 rust-regex-automata-0.4.18
                                                 rust-regex-syntax-0.8.11
                                                 rust-reqwest-0.12.28
                                                 rust-ring-0.17.14
                                                 rust-rustc-hash-2.1.3
                                                 rust-rustix-1.1.4
                                                 rust-rustls-0.21.12
                                                 rust-rustls-0.23.43
                                                 rust-rustls-native-certs-0.6.3
                                                 rust-rustls-pemfile-1.0.4
                                                 rust-rustls-pki-types-1.15.1
                                                 rust-rustls-webpki-0.101.7
                                                 rust-rustls-webpki-0.103.14
                                                 rust-rustversion-1.0.23
                                                 rust-ryu-1.0.23
                                                 rust-schannel-0.1.29
                                                 rust-sct-0.7.1
                                                 rust-security-framework-2.11.1
                                                 rust-security-framework-sys-2.17.0
                                                 rust-serde-1.0.229
                                                 rust-serde-core-1.0.229
                                                 rust-serde-derive-1.0.229
                                                 rust-serde-json-1.0.151
                                                 rust-serde-urlencoded-0.7.1
                                                 rust-sha1-0.10.7
                                                 rust-shlex-2.0.1
                                                 rust-simd-adler32-0.3.10
                                                 rust-slab-0.4.12
                                                 rust-smallvec-1.15.2
                                                 rust-socket2-0.6.5
                                                 rust-sophon-lib-0.1.8.58d223a
                                                 rust-stable-deref-trait-1.2.1
                                                 rust-strsim-0.11.1
                                                 rust-subtle-2.6.1
                                                 rust-syn-2.0.119
                                                 rust-syn-3.0.3
                                                 rust-sync-wrapper-1.0.2
                                                 rust-synstructure-0.13.2
                                                 rust-sysinfo-0.35.2
                                                 rust-tar-0.4.46
                                                 rust-tempfile-3.27.0
                                                 rust-thiserror-1.0.69
                                                 rust-thiserror-2.0.20
                                                 rust-thiserror-impl-1.0.69
                                                 rust-thiserror-impl-2.0.20
                                                 rust-time-0.3.55
                                                 rust-time-core-0.1.9
                                                 rust-tinystr-0.8.4
                                                 rust-tinyvec-1.12.0
                                                 rust-tinyvec-macros-0.1.1
                                                 rust-tokio-1.53.1
                                                 rust-tokio-rustls-0.26.4
                                                 rust-tokio-util-0.7.19
                                                 rust-tower-0.5.3
                                                 rust-tower-http-0.6.11
                                                 rust-tower-layer-0.3.3
                                                 rust-tower-service-0.3.3
                                                 rust-tracing-0.1.44
                                                 rust-tracing-attributes-0.1.31
                                                 rust-tracing-core-0.1.36
                                                 rust-try-lock-0.2.5
                                                 rust-typenum-1.20.1
                                                 rust-unicode-ident-1.0.24
                                                 rust-untrusted-0.9.0
                                                 rust-url-2.5.8
                                                 rust-utf8-iter-1.0.4
                                                 rust-version-check-0.9.5
                                                 rust-want-0.3.1
                                                 rust-wasi-0.11.1+wasi-snapshot-preview1
                                                 rust-wasip2-1.0.4+wasi-0.2.12
                                                 rust-wasm-bindgen-0.2.127
                                                 rust-wasm-bindgen-futures-0.4.77
                                                 rust-wasm-bindgen-macro-0.2.127
                                                 rust-wasm-bindgen-macro-support-0.2.127
                                                 rust-wasm-bindgen-shared-0.2.127
                                                 rust-web-sys-0.3.104
                                                 rust-web-time-1.1.0
                                                 rust-webpki-roots-0.25.4
                                                 rust-webpki-roots-1.0.9
                                                 rust-widestring-1.2.1
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
                                                 rust-wit-bindgen-0.57.1
                                                 rust-writeable-0.6.4
                                                 rust-xattr-1.6.1
                                                 rust-xz-0.1.0
                                                 rust-xz2-0.1.7
                                                 rust-yoke-0.8.3
                                                 rust-yoke-derive-0.8.2
                                                 rust-zerocopy-0.8.56
                                                 rust-zerocopy-derive-0.8.56
                                                 rust-zerofrom-0.1.8
                                                 rust-zerofrom-derive-0.1.7
                                                 rust-zeroize-1.9.0
                                                 rust-zeroize-derive-1.5.0
                                                 rust-zerotrie-0.2.5
                                                 rust-zerovec-0.11.7
                                                 rust-zerovec-derive-0.11.4
                                                 rust-zip-3.0.0
                                                 rust-zlib-rs-0.6.7
                                                 rust-zmij-1.0.23
                                                 rust-zopfli-0.8.3
                                                 rust-zstd-0.13.3
                                                 rust-zstd-safe-7.2.4
                                                 rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (anime-launcher-sdk-1.36.7 =>
                                                (list rust-adler2-2.0.1
                                                 rust-aes-0.8.4
                                                 rust-ahash-0.8.12
                                                 rust-aho-corasick-1.1.5
                                                 rust-allocator-api2-0.2.21
                                                 rust-anime-game-core-1.39.3.f1f0a23
                                                 rust-anyhow-1.0.104
                                                 rust-arbitrary-1.4.2
                                                 rust-arrayvec-0.7.8
                                                 rust-atomic-waker-1.1.2
                                                 rust-base64-0.21.7
                                                 rust-base64-0.22.1
                                                 rust-bitflags-2.13.1
                                                 rust-blake3-1.8.7
                                                 rust-block-buffer-0.10.4
                                                 rust-bumpalo-3.20.3
                                                 rust-byteorder-1.5.0
                                                 rust-bytes-1.12.1
                                                 rust-bzip2-0.4.4
                                                 rust-bzip2-0.5.2
                                                 rust-bzip2-sys-0.1.13+1.0.8
                                                 rust-cached-0.55.1
                                                 rust-cached-proc-macro-0.24.0
                                                 rust-cached-proc-macro-types-0.1.1
                                                 rust-cc-1.4.4
                                                 rust-cfg-if-1.0.4
                                                 rust-cfg-aliases-0.2.2
                                                 rust-chacha20-0.10.1
                                                 rust-cipher-0.4.4
                                                 rust-constant-time-eq-0.3.1
                                                 rust-constant-time-eq-0.4.2
                                                 rust-core-foundation-0.9.4
                                                 rust-core-foundation-sys-0.8.7
                                                 rust-cpufeatures-0.2.17
                                                 rust-cpufeatures-0.3.0
                                                 rust-crc-3.4.0
                                                 rust-crc-catalog-2.5.0
                                                 rust-crc32fast-1.5.1
                                                 rust-crossbeam-channel-0.5.16
                                                 rust-crossbeam-utils-0.8.22
                                                 rust-crypto-common-0.1.7
                                                 rust-darling-0.20.11
                                                 rust-darling-core-0.20.11
                                                 rust-darling-macro-0.20.11
                                                 rust-deflate64-0.1.12
                                                 rust-deranged-0.5.8
                                                 rust-derive-arbitrary-1.4.2
                                                 rust-digest-0.10.7
                                                 rust-displaydoc-0.2.7
                                                 rust-dns-lookup-2.1.1
                                                 rust-doctest-file-1.1.1
                                                 rust-either-1.18.0
                                                 rust-enum-ordinalize-4.4.2
                                                 rust-enum-ordinalize-derive-4.4.2
                                                 rust-equivalent-1.0.2
                                                 rust-errno-0.3.14
                                                 rust-fastrand-2.5.0
                                                 rust-filetime-0.2.29
                                                 rust-find-msvc-tools-0.1.11
                                                 rust-fixedbitset-0.5.7
                                                 rust-flate2-1.1.9
                                                 rust-fnv-1.0.7
                                                 rust-foldhash-0.1.5
                                                 rust-form-urlencoded-1.2.2
                                                 rust-fs2-0.4.3
                                                 rust-fs-extra-1.3.0
                                                 rust-futures-channel-0.3.34
                                                 rust-futures-core-0.3.34
                                                 rust-futures-io-0.3.34
                                                 rust-futures-sink-0.3.34
                                                 rust-futures-task-0.3.34
                                                 rust-futures-util-0.3.34
                                                 rust-generic-array-0.14.7
                                                 rust-getrandom-0.2.17
                                                 rust-getrandom-0.3.4
                                                 rust-getrandom-0.4.3
                                                 rust-h2-0.4.19
                                                 rust-hashbrown-0.14.5
                                                 rust-hashbrown-0.15.5
                                                 rust-hashbrown-0.17.1
                                                 rust-heck-0.5.0
                                                 rust-hmac-0.12.1
                                                 rust-http-1.5.0
                                                 rust-http-body-1.1.0
                                                 rust-http-body-util-0.1.5
                                                 rust-httparse-1.10.1
                                                 rust-hyper-1.11.0
                                                 rust-hyper-rustls-0.27.9
                                                 rust-hyper-util-0.1.20
                                                 rust-icu-collections-2.3.0
                                                 rust-icu-locale-core-2.3.0
                                                 rust-icu-normalizer-2.3.0
                                                 rust-icu-normalizer-data-2.3.0
                                                 rust-icu-properties-2.3.0
                                                 rust-icu-properties-data-2.3.0
                                                 rust-icu-provider-2.3.1
                                                 rust-ident-case-1.0.1
                                                 rust-idna-1.1.0
                                                 rust-idna-adapter-1.2.2
                                                 rust-indexmap-2.14.0
                                                 rust-inout-0.1.4
                                                 rust-interprocess-2.4.3
                                                 rust-ipnet-2.12.1
                                                 rust-itertools-0.14.0
                                                 rust-itoa-1.0.18
                                                 rust-jobserver-0.1.35
                                                 rust-js-sys-0.3.104
                                                 rust-kinda-virtual-fs-0.1.1
                                                 rust-lazy-static-1.5.0
                                                 rust-libc-0.2.189
                                                 rust-linux-raw-sys-0.12.1
                                                 rust-litemap-0.8.3
                                                 rust-log-0.4.34
                                                 rust-lru-slab-0.1.2
                                                 rust-lzma-rs-0.3.0
                                                 rust-lzma-sys-0.1.20
                                                 rust-md-5-0.10.6
                                                 rust-md5-asm-0.5.2
                                                 rust-memchr-2.8.3
                                                 rust-miniz-oxide-0.8.9
                                                 rust-minreq-2.14.1
                                                 rust-mio-1.2.2
                                                 rust-multimap-0.10.1
                                                 rust-ntapi-0.4.3
                                                 rust-num-conv-0.2.2
                                                 rust-objc2-core-foundation-0.3.2
                                                 rust-objc2-io-kit-0.3.2
                                                 rust-once-cell-1.21.4
                                                 rust-openssl-probe-0.1.6
                                                 rust-pbkdf2-0.12.2
                                                 rust-percent-encoding-2.3.2
                                                 rust-petgraph-0.8.3
                                                 rust-pin-project-lite-0.2.17
                                                 rust-pkg-config-0.3.34
                                                 rust-potential-utf-0.1.6
                                                 rust-powerfmt-0.2.0
                                                 rust-prettyplease-0.2.37
                                                 rust-proc-macro2-1.0.107
                                                 rust-prost-0.14.4
                                                 rust-prost-build-0.14.4
                                                 rust-prost-derive-0.14.4
                                                 rust-prost-types-0.14.4
                                                 rust-quinn-0.11.11
                                                 rust-quinn-proto-0.11.17
                                                 rust-quinn-udp-0.5.15
                                                 rust-quote-1.0.47
                                                 rust-r-efi-5.3.0
                                                 rust-r-efi-6.0.0
                                                 rust-rand-0.10.2
                                                 rust-rand-core-0.10.1
                                                 rust-rand-pcg-0.10.2
                                                 rust-recvmsg-1.0.0
                                                 rust-regex-1.13.1
                                                 rust-regex-automata-0.4.18
                                                 rust-regex-syntax-0.8.11
                                                 rust-reqwest-0.12.28
                                                 rust-ring-0.17.14
                                                 rust-rustc-hash-2.1.3
                                                 rust-rustix-1.1.4
                                                 rust-rustls-0.21.12
                                                 rust-rustls-0.23.43
                                                 rust-rustls-native-certs-0.6.3
                                                 rust-rustls-pemfile-1.0.4
                                                 rust-rustls-pki-types-1.15.1
                                                 rust-rustls-webpki-0.101.7
                                                 rust-rustls-webpki-0.103.15
                                                 rust-rustversion-1.0.23
                                                 rust-ryu-1.0.23
                                                 rust-schannel-0.1.29
                                                 rust-sct-0.7.1
                                                 rust-security-framework-2.11.1
                                                 rust-security-framework-sys-2.17.0
                                                 rust-serde-1.0.229
                                                 rust-serde-core-1.0.229
                                                 rust-serde-derive-1.0.229
                                                 rust-serde-json-1.0.151
                                                 rust-serde-urlencoded-0.7.1
                                                 rust-sha1-0.10.7
                                                 rust-shlex-2.0.1
                                                 rust-simd-adler32-0.3.10
                                                 rust-slab-0.4.12
                                                 rust-smallvec-1.15.2
                                                 rust-socket2-0.6.5
                                                 rust-sophon-lib-0.1.9.f422286
                                                 rust-stable-deref-trait-1.2.1
                                                 rust-strsim-0.11.1
                                                 rust-subtle-2.6.1
                                                 rust-syn-2.0.119
                                                 rust-syn-3.0.4
                                                 rust-sync-wrapper-1.0.2
                                                 rust-synstructure-0.13.2
                                                 rust-sysinfo-0.35.2
                                                 rust-tar-0.4.46
                                                 rust-tempfile-3.27.0
                                                 rust-thiserror-1.0.69
                                                 rust-thiserror-2.0.20
                                                 rust-thiserror-impl-1.0.69
                                                 rust-thiserror-impl-2.0.20
                                                 rust-time-0.3.55
                                                 rust-time-core-0.1.9
                                                 rust-tinystr-0.8.4
                                                 rust-tinyvec-1.12.0
                                                 rust-tinyvec-macros-0.1.1
                                                 rust-tokio-1.53.1
                                                 rust-tokio-rustls-0.26.4
                                                 rust-tokio-util-0.7.19
                                                 rust-tower-0.5.3
                                                 rust-tower-http-0.6.11
                                                 rust-tower-layer-0.3.3
                                                 rust-tower-service-0.3.3
                                                 rust-tracing-0.1.44
                                                 rust-tracing-attributes-0.1.31
                                                 rust-tracing-core-0.1.36
                                                 rust-try-lock-0.2.5
                                                 rust-typenum-1.20.1
                                                 rust-unicode-ident-1.0.24
                                                 rust-untrusted-0.9.0
                                                 rust-url-2.5.8
                                                 rust-utf8-iter-1.0.4
                                                 rust-version-check-0.9.5
                                                 rust-want-0.3.1
                                                 rust-wasi-0.11.1+wasi-snapshot-preview1
                                                 rust-wasip2-1.0.4+wasi-0.2.12
                                                 rust-wasm-bindgen-0.2.127
                                                 rust-wasm-bindgen-futures-0.4.77
                                                 rust-wasm-bindgen-macro-0.2.127
                                                 rust-wasm-bindgen-macro-support-0.2.127
                                                 rust-wasm-bindgen-shared-0.2.127
                                                 rust-web-sys-0.3.104
                                                 rust-web-time-1.1.0
                                                 rust-webpki-roots-0.25.4
                                                 rust-webpki-roots-1.0.9
                                                 rust-widestring-1.2.1
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
                                                 rust-wit-bindgen-0.57.1
                                                 rust-writeable-0.6.4
                                                 rust-xattr-1.6.1
                                                 rust-xz-0.1.0
                                                 rust-xz2-0.1.7
                                                 rust-yoke-0.8.3
                                                 rust-yoke-derive-0.8.2
                                                 rust-zerocopy-0.8.56
                                                 rust-zerocopy-derive-0.8.56
                                                 rust-zerofrom-0.1.8
                                                 rust-zerofrom-derive-0.1.7
                                                 rust-zeroize-1.9.0
                                                 rust-zeroize-derive-1.5.0
                                                 rust-zerotrie-0.2.5
                                                 rust-zerovec-0.11.8
                                                 rust-zerovec-derive-0.11.6
                                                 rust-zip-3.0.0
                                                 rust-zlib-rs-0.6.7
                                                 rust-zmij-1.0.23
                                                 rust-zopfli-0.8.3
                                                 rust-zstd-0.13.3
                                                 rust-zstd-safe-7.2.4
                                                 rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (honkers-launcher =>
                                       (list rust-addr2line-0.25.1
                                        rust-adler2-2.0.1
                                        rust-aes-0.8.4
                                        rust-ahash-0.8.12
                                        rust-aho-corasick-1.1.5
                                        rust-allocator-api2-0.2.21
                                        rust-anime-game-core-1.39.3.f1f0a23
                                        rust-anime-launcher-sdk-1.36.7.c682598
                                        rust-anstream-1.0.0
                                        rust-anstyle-1.0.14
                                        rust-anstyle-parse-1.0.0
                                        rust-anstyle-query-1.1.5
                                        rust-anstyle-wincon-3.0.11
                                        rust-anyhow-1.0.104
                                        rust-arbitrary-1.4.2
                                        rust-arrayvec-0.7.8
                                        rust-ashpd-0.11.1
                                        rust-async-broadcast-0.7.2
                                        rust-async-recursion-1.1.1
                                        rust-async-trait-0.1.92
                                        rust-atomic-waker-1.1.2
                                        rust-autocfg-1.5.1
                                        rust-backtrace-0.3.76
                                        rust-base64-0.21.7
                                        rust-base64-0.22.1
                                        rust-bitflags-2.13.1
                                        rust-blake3-1.8.7
                                        rust-block-buffer-0.10.4
                                        rust-block2-0.6.2
                                        rust-bstr-1.13.1
                                        rust-bumpalo-3.20.3
                                        rust-byteorder-1.5.0
                                        rust-bytes-1.12.1
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
                                        rust-cc-1.4.4
                                        rust-cfg-expr-0.20.9
                                        rust-cfg-if-1.0.4
                                        rust-cfg-aliases-0.2.2
                                        rust-chacha20-0.10.1
                                        rust-cipher-0.4.4
                                        rust-colorchoice-1.0.5
                                        rust-constant-time-eq-0.3.1
                                        rust-constant-time-eq-0.4.2
                                        rust-core-foundation-0.9.4
                                        rust-core-foundation-sys-0.8.7
                                        rust-cpufeatures-0.2.17
                                        rust-cpufeatures-0.3.0
                                        rust-crc-3.4.0
                                        rust-crc-catalog-2.5.0
                                        rust-crc32fast-1.5.1
                                        rust-crossbeam-channel-0.5.16
                                        rust-crossbeam-deque-0.8.7
                                        rust-crossbeam-epoch-0.9.20
                                        rust-crossbeam-utils-0.8.22
                                        rust-crypto-common-0.1.7
                                        rust-darling-0.20.11
                                        rust-darling-core-0.20.11
                                        rust-darling-macro-0.20.11
                                        rust-deflate64-0.1.12
                                        rust-deranged-0.5.8
                                        rust-derive-arbitrary-1.4.2
                                        rust-digest-0.10.7
                                        rust-dispatch2-0.3.1
                                        rust-displaydoc-0.2.7
                                        rust-dlib-0.5.3
                                        rust-dns-lookup-2.1.1
                                        rust-doctest-file-1.1.1
                                        rust-downcast-rs-1.2.1
                                        rust-either-1.18.0
                                        rust-endi-1.1.1
                                        rust-enum-ordinalize-4.4.2
                                        rust-enum-ordinalize-derive-4.4.2
                                        rust-enumflags2-0.7.12
                                        rust-enumflags2-derive-0.7.12
                                        rust-equivalent-1.0.2
                                        rust-errno-0.3.14
                                        rust-event-listener-5.4.2
                                        rust-event-listener-strategy-0.5.4
                                        rust-fastrand-2.5.0
                                        rust-field-offset-0.3.6
                                        rust-filetime-0.2.29
                                        rust-find-msvc-tools-0.1.11
                                        rust-fixedbitset-0.5.7
                                        rust-flate2-1.1.9
                                        rust-fluent-bundle-0.15.3
                                        rust-fluent-langneg-0.13.1
                                        rust-fluent-syntax-0.11.1
                                        rust-fluent-template-macros-0.11.0
                                        rust-fluent-templates-0.11.0
                                        rust-flume-0.11.1
                                        rust-fnv-1.0.7
                                        rust-foldhash-0.1.5
                                        rust-form-urlencoded-1.2.2
                                        rust-fragile-2.1.0
                                        rust-fs2-0.4.3
                                        rust-fs-extra-1.3.0
                                        rust-futures-0.3.34
                                        rust-futures-channel-0.3.34
                                        rust-futures-core-0.3.34
                                        rust-futures-executor-0.3.34
                                        rust-futures-io-0.3.34
                                        rust-futures-lite-2.6.1
                                        rust-futures-macro-0.3.34
                                        rust-futures-sink-0.3.34
                                        rust-futures-task-0.3.34
                                        rust-futures-util-0.3.34
                                        rust-gdk-pixbuf-0.20.10
                                        rust-gdk-pixbuf-sys-0.20.10
                                        rust-gdk4-0.9.6
                                        rust-gdk4-sys-0.9.6
                                        rust-generic-array-0.14.7
                                        rust-getrandom-0.2.17
                                        rust-getrandom-0.3.4
                                        rust-getrandom-0.4.3
                                        rust-gimli-0.32.3
                                        rust-gio-0.20.12
                                        rust-gio-sys-0.20.10
                                        rust-glib-0.20.12
                                        rust-glib-build-tools-0.20.0
                                        rust-glib-macros-0.20.12
                                        rust-glib-sys-0.20.10
                                        rust-globset-0.4.20
                                        rust-gobject-sys-0.20.10
                                        rust-graphene-rs-0.20.10
                                        rust-graphene-sys-0.20.10
                                        rust-gsk4-0.9.6
                                        rust-gsk4-sys-0.9.6
                                        rust-gtk4-0.9.7
                                        rust-gtk4-macros-0.9.5
                                        rust-gtk4-sys-0.9.6
                                        rust-h2-0.4.19
                                        rust-hashbrown-0.14.5
                                        rust-hashbrown-0.15.5
                                        rust-hashbrown-0.17.1
                                        rust-heck-0.5.0
                                        rust-hex-0.4.3
                                        rust-hmac-0.12.1
                                        rust-http-1.5.0
                                        rust-http-body-1.1.0
                                        rust-http-body-util-0.1.5
                                        rust-httparse-1.10.1
                                        rust-human-panic-2.0.8
                                        rust-hyper-1.11.0
                                        rust-hyper-rustls-0.27.9
                                        rust-hyper-util-0.1.20
                                        rust-icu-collections-2.3.0
                                        rust-icu-locale-core-2.3.0
                                        rust-icu-normalizer-2.3.0
                                        rust-icu-normalizer-data-2.3.0
                                        rust-icu-properties-2.3.0
                                        rust-icu-properties-data-2.3.0
                                        rust-icu-provider-2.3.1
                                        rust-ident-case-1.0.1
                                        rust-idna-1.1.0
                                        rust-idna-adapter-1.2.2
                                        rust-ignore-0.4.33
                                        rust-indexmap-2.14.0
                                        rust-inout-0.1.4
                                        rust-interprocess-2.4.3
                                        rust-intl-memoizer-0.5.3
                                        rust-intl-pluralrules-7.0.2
                                        rust-ipnet-2.12.1
                                        rust-is-docker-0.2.0
                                        rust-is-wsl-0.4.0
                                        rust-is-terminal-polyfill-1.70.2
                                        rust-itertools-0.14.0
                                        rust-itoa-1.0.18
                                        rust-jobserver-0.1.35
                                        rust-js-sys-0.3.104
                                        rust-kinda-virtual-fs-0.1.1
                                        rust-lazy-static-1.5.0
                                        rust-libadwaita-0.7.2
                                        rust-libadwaita-sys-0.7.2
                                        rust-libc-0.2.189
                                        rust-libloading-0.8.9
                                        rust-linux-raw-sys-0.12.1
                                        rust-litemap-0.8.3
                                        rust-lock-api-0.4.14
                                        rust-log-0.4.34
                                        rust-lru-slab-0.1.2
                                        rust-lzma-rs-0.3.0
                                        rust-lzma-sys-0.1.20
                                        rust-md-5-0.10.6
                                        rust-md5-asm-0.5.2
                                        rust-memchr-2.8.3
                                        rust-memoffset-0.9.1
                                        rust-miniz-oxide-0.8.9
                                        rust-minreq-2.14.1
                                        rust-mio-1.2.2
                                        rust-multimap-0.10.1
                                        rust-nanorand-0.7.0
                                        rust-ntapi-0.4.3
                                        rust-nu-ansi-term-0.50.3
                                        rust-num-conv-0.2.2
                                        rust-objc2-0.6.4
                                        rust-objc2-app-kit-0.3.2
                                        rust-objc2-core-foundation-0.3.2
                                        rust-objc2-encode-4.1.0
                                        rust-objc2-foundation-0.3.2
                                        rust-objc2-io-kit-0.3.2
                                        rust-object-0.37.3
                                        rust-once-cell-1.21.4
                                        rust-once-cell-polyfill-1.70.2
                                        rust-open-5.4.2
                                        rust-openssl-probe-0.1.6
                                        rust-ordered-stream-0.2.0
                                        rust-pango-0.20.12
                                        rust-pango-sys-0.20.10
                                        rust-parking-2.2.1
                                        rust-pbkdf2-0.12.2
                                        rust-percent-encoding-2.3.2
                                        rust-petgraph-0.8.3
                                        rust-pin-project-lite-0.2.17
                                        rust-pkg-config-0.3.34
                                        rust-pollster-0.4.0
                                        rust-potential-utf-0.1.6
                                        rust-powerfmt-0.2.0
                                        rust-ppv-lite86-0.2.21
                                        rust-prettyplease-0.2.37
                                        rust-proc-macro-crate-3.5.0
                                        rust-proc-macro-hack-0.5.20+deprecated
                                        rust-proc-macro2-1.0.107
                                        rust-prost-0.14.4
                                        rust-prost-build-0.14.4
                                        rust-prost-derive-0.14.4
                                        rust-prost-types-0.14.4
                                        rust-quick-xml-0.41.0
                                        rust-quinn-0.11.11
                                        rust-quinn-proto-0.11.17
                                        rust-quinn-udp-0.5.15
                                        rust-quote-1.0.47
                                        rust-r-efi-5.3.0
                                        rust-r-efi-6.0.0
                                        rust-rand-0.9.5
                                        rust-rand-0.10.2
                                        rust-rand-chacha-0.9.0
                                        rust-rand-core-0.9.5
                                        rust-rand-core-0.10.1
                                        rust-rand-pcg-0.10.2
                                        rust-raw-window-handle-0.6.2
                                        rust-recvmsg-1.0.0
                                        rust-regex-1.13.1
                                        rust-regex-automata-0.4.18
                                        rust-regex-syntax-0.8.11
                                        rust-relm4-0.9.1
                                        rust-relm4-css-0.9.0
                                        rust-relm4-macros-0.9.1
                                        rust-reqwest-0.12.28
                                        rust-rfd-0.15.4
                                        rust-ring-0.17.14
                                        rust-rustc-demangle-0.1.28
                                        rust-rustc-hash-1.1.0
                                        rust-rustc-hash-2.1.3
                                        rust-rustc-version-0.4.1
                                        rust-rustix-1.1.4
                                        rust-rustls-0.21.12
                                        rust-rustls-0.23.43
                                        rust-rustls-native-certs-0.6.3
                                        rust-rustls-pemfile-1.0.4
                                        rust-rustls-pki-types-1.15.1
                                        rust-rustls-webpki-0.101.7
                                        rust-rustls-webpki-0.103.15
                                        rust-rustversion-1.0.23
                                        rust-ryu-1.0.23
                                        rust-same-file-1.0.6
                                        rust-schannel-0.1.29
                                        rust-scoped-tls-1.0.1
                                        rust-scopeguard-1.2.0
                                        rust-sct-0.7.1
                                        rust-security-framework-2.11.1
                                        rust-security-framework-sys-2.17.0
                                        rust-self-cell-0.10.3
                                        rust-self-cell-1.3.0
                                        rust-semver-1.0.28
                                        rust-serde-1.0.229
                                        rust-serde-core-1.0.229
                                        rust-serde-derive-1.0.229
                                        rust-serde-json-1.0.151
                                        rust-serde-repr-0.1.21
                                        rust-serde-spanned-1.1.1
                                        rust-serde-urlencoded-0.7.1
                                        rust-sha1-0.10.7
                                        rust-sharded-slab-0.1.7
                                        rust-shlex-2.0.1
                                        rust-signal-hook-registry-1.4.8
                                        rust-simd-adler32-0.3.10
                                        rust-slab-0.4.12
                                        rust-smallvec-1.15.2
                                        rust-socket2-0.6.5
                                        rust-sophon-lib-0.1.9.f422286
                                        rust-spin-0.9.9
                                        rust-stable-deref-trait-1.2.1
                                        rust-strsim-0.11.1
                                        rust-subtle-2.6.1
                                        rust-syn-2.0.119
                                        rust-syn-3.0.4
                                        rust-sync-wrapper-1.0.2
                                        rust-synstructure-0.13.2
                                        rust-sysinfo-0.35.2
                                        rust-sysinfo-0.38.4
                                        rust-system-deps-7.0.8
                                        rust-tar-0.4.46
                                        rust-target-lexicon-0.13.5
                                        rust-tempfile-3.27.0
                                        rust-thiserror-1.0.69
                                        rust-thiserror-2.0.20
                                        rust-thiserror-impl-1.0.69
                                        rust-thiserror-impl-2.0.20
                                        rust-thread-local-1.1.10
                                        rust-time-0.3.55
                                        rust-time-core-0.1.9
                                        rust-tinystr-0.8.4
                                        rust-tinyvec-1.12.0
                                        rust-tinyvec-macros-0.1.1
                                        rust-tokio-1.53.1
                                        rust-tokio-rustls-0.26.4
                                        rust-tokio-util-0.7.19
                                        rust-toml-1.1.4+spec-1.1.0
                                        rust-toml-datetime-1.1.1+spec-1.1.0
                                        rust-toml-edit-0.25.13+spec-1.1.0
                                        rust-toml-parser-1.1.3+spec-1.1.0
                                        rust-toml-writer-1.1.2+spec-1.1.0
                                        rust-tower-0.5.3
                                        rust-tower-http-0.6.11
                                        rust-tower-layer-0.3.3
                                        rust-tower-service-0.3.3
                                        rust-tracing-0.1.44
                                        rust-tracing-attributes-0.1.31
                                        rust-tracing-core-0.1.36
                                        rust-tracing-log-0.2.0
                                        rust-tracing-subscriber-0.3.23
                                        rust-try-lock-0.2.5
                                        rust-type-map-0.5.1
                                        rust-typenum-1.20.1
                                        rust-uds-windows-1.2.1
                                        rust-unic-langid-0.9.6
                                        rust-unic-langid-impl-0.9.6
                                        rust-unic-langid-macros-0.9.6
                                        rust-unic-langid-macros-impl-0.9.6
                                        rust-unicode-ident-1.0.24
                                        rust-untrusted-0.9.0
                                        rust-url-2.5.8
                                        rust-urlencoding-2.1.3
                                        rust-utf8-iter-1.0.4
                                        rust-utf8parse-0.2.2
                                        rust-uuid-1.25.0
                                        rust-valuable-0.1.1
                                        rust-version-compare-0.2.1
                                        rust-version-check-0.9.5
                                        rust-walkdir-2.5.0
                                        rust-want-0.3.1
                                        rust-wasi-0.11.1+wasi-snapshot-preview1
                                        rust-wasip2-1.0.4+wasi-0.2.12
                                        rust-wasm-bindgen-0.2.127
                                        rust-wasm-bindgen-futures-0.4.77
                                        rust-wasm-bindgen-macro-0.2.127
                                        rust-wasm-bindgen-macro-support-0.2.127
                                        rust-wasm-bindgen-shared-0.2.127
                                        rust-wayland-backend-0.3.17
                                        rust-wayland-client-0.31.15
                                        rust-wayland-protocols-0.32.13
                                        rust-wayland-scanner-0.31.11
                                        rust-wayland-sys-0.31.11
                                        rust-web-sys-0.3.104
                                        rust-web-time-1.1.0
                                        rust-webpki-roots-0.25.4
                                        rust-webpki-roots-1.0.9
                                        rust-whatadistro-0.1.0
                                        rust-widestring-1.2.1
                                        rust-winapi-0.3.9
                                        rust-winapi-i686-pc-windows-gnu-0.4.0
                                        rust-winapi-util-0.1.11
                                        rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                        rust-wincompatlib-0.7.7
                                        rust-windows-0.61.3
                                        rust-windows-0.62.2
                                        rust-windows-collections-0.2.0
                                        rust-windows-collections-0.3.2
                                        rust-windows-core-0.61.2
                                        rust-windows-core-0.62.2
                                        rust-windows-future-0.2.1
                                        rust-windows-future-0.3.2
                                        rust-windows-implement-0.60.2
                                        rust-windows-interface-0.59.3
                                        rust-windows-link-0.1.3
                                        rust-windows-link-0.2.1
                                        rust-windows-numerics-0.2.0
                                        rust-windows-numerics-0.3.1
                                        rust-windows-result-0.3.4
                                        rust-windows-result-0.4.1
                                        rust-windows-strings-0.4.2
                                        rust-windows-strings-0.5.1
                                        rust-windows-sys-0.52.0
                                        rust-windows-sys-0.59.0
                                        rust-windows-sys-0.60.2
                                        rust-windows-sys-0.61.2
                                        rust-windows-targets-0.52.6
                                        rust-windows-targets-0.53.5
                                        rust-windows-threading-0.1.0
                                        rust-windows-threading-0.2.1
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
                                        rust-winnow-1.0.4
                                        rust-wit-bindgen-0.57.1
                                        rust-writeable-0.6.4
                                        rust-xattr-1.6.1
                                        rust-xz-0.1.0
                                        rust-xz2-0.1.7
                                        rust-yoke-0.8.3
                                        rust-yoke-derive-0.8.2
                                        rust-zbus-5.19.0
                                        rust-zbus-macros-5.19.0
                                        rust-zbus-names-4.3.4
                                        rust-zcheapstr-1.1.0
                                        rust-zerocopy-0.8.56
                                        rust-zerocopy-derive-0.8.56
                                        rust-zerofrom-0.1.8
                                        rust-zerofrom-derive-0.1.7
                                        rust-zeroize-1.9.0
                                        rust-zeroize-derive-1.5.0
                                        rust-zerotrie-0.2.5
                                        rust-zerovec-0.11.8
                                        rust-zerovec-derive-0.11.6
                                        rust-zip-3.0.0
                                        rust-zlib-rs-0.6.7
                                        rust-zmij-1.0.23
                                        rust-zopfli-0.8.3
                                        rust-zstd-0.13.3
                                        rust-zstd-safe-7.2.4
                                        rust-zstd-sys-2.0.16+zstd.1.5.7
                                        rust-zvariant-5.15.0
                                        rust-zvariant-derive-5.15.0
                                        rust-zvariant-utils-4.2.0))
                     (sleepy-launcher =>
                                      (list rust-addr2line-0.25.1
                                       rust-adler2-2.0.1
                                       rust-aes-0.8.4
                                       rust-ahash-0.8.12
                                       rust-aho-corasick-1.1.5
                                       rust-allocator-api2-0.2.21
                                       rust-anime-game-core-1.39.1.aa8c5ce
                                       rust-anime-launcher-sdk-1.36.4.bf66ccb
                                       rust-anstream-1.0.0
                                       rust-anstyle-1.0.14
                                       rust-anstyle-parse-1.0.0
                                       rust-anstyle-query-1.1.5
                                       rust-anstyle-wincon-3.0.11
                                       rust-anyhow-1.0.104
                                       rust-arbitrary-1.4.2
                                       rust-arrayref-0.3.9
                                       rust-arrayvec-0.7.8
                                       rust-ashpd-0.11.1
                                       rust-async-broadcast-0.7.2
                                       rust-async-recursion-1.1.1
                                       rust-async-trait-0.1.92
                                       rust-autocfg-1.5.1
                                       rust-backtrace-0.3.76
                                       rust-base64-0.21.7
                                       rust-base64-0.22.1
                                       rust-bitflags-2.13.1
                                       rust-blake3-1.8.6
                                       rust-block-buffer-0.10.4
                                       rust-block2-0.6.2
                                       rust-bstr-1.13.1
                                       rust-bumpalo-3.20.3
                                       rust-byteorder-1.5.0
                                       rust-bytes-1.12.1
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
                                       rust-cc-1.4.2
                                       rust-cfg-expr-0.20.8
                                       rust-cfg-if-1.0.4
                                       rust-cipher-0.4.4
                                       rust-colorchoice-1.0.5
                                       rust-constant-time-eq-0.3.1
                                       rust-constant-time-eq-0.4.2
                                       rust-core-foundation-0.9.4
                                       rust-core-foundation-sys-0.8.7
                                       rust-cpufeatures-0.2.17
                                       rust-cpufeatures-0.3.0
                                       rust-crc-3.4.0
                                       rust-crc-catalog-2.5.0
                                       rust-crc32fast-1.5.0
                                       rust-crossbeam-deque-0.8.7
                                       rust-crossbeam-epoch-0.9.20
                                       rust-crossbeam-utils-0.8.22
                                       rust-crypto-common-0.1.7
                                       rust-darling-0.20.11
                                       rust-darling-core-0.20.11
                                       rust-darling-macro-0.20.11
                                       rust-deflate64-0.1.12
                                       rust-deranged-0.5.8
                                       rust-derive-arbitrary-1.4.2
                                       rust-digest-0.10.7
                                       rust-dispatch2-0.3.1
                                       rust-displaydoc-0.2.7
                                       rust-dlib-0.5.3
                                       rust-dns-lookup-2.1.1
                                       rust-downcast-rs-1.2.1
                                       rust-endi-1.1.1
                                       rust-enum-ordinalize-4.4.2
                                       rust-enum-ordinalize-derive-4.4.2
                                       rust-enumflags2-0.7.12
                                       rust-enumflags2-derive-0.7.12
                                       rust-equivalent-1.0.2
                                       rust-errno-0.3.14
                                       rust-event-listener-5.4.2
                                       rust-event-listener-strategy-0.5.4
                                       rust-fastrand-2.5.0
                                       rust-field-offset-0.3.6
                                       rust-filetime-0.2.29
                                       rust-find-msvc-tools-0.1.10
                                       rust-flate2-1.1.9
                                       rust-fluent-bundle-0.15.3
                                       rust-fluent-langneg-0.13.1
                                       rust-fluent-syntax-0.11.1
                                       rust-fluent-template-macros-0.11.0
                                       rust-fluent-templates-0.11.0
                                       rust-flume-0.11.1
                                       rust-fnv-1.0.7
                                       rust-form-urlencoded-1.2.2
                                       rust-fragile-2.1.0
                                       rust-fs-extra-1.3.0
                                       rust-futures-0.3.34
                                       rust-futures-channel-0.3.34
                                       rust-futures-core-0.3.34
                                       rust-futures-executor-0.3.34
                                       rust-futures-io-0.3.34
                                       rust-futures-lite-2.6.1
                                       rust-futures-macro-0.3.34
                                       rust-futures-sink-0.3.34
                                       rust-futures-task-0.3.34
                                       rust-futures-util-0.3.34
                                       rust-gdk-pixbuf-0.20.10
                                       rust-gdk-pixbuf-sys-0.20.10
                                       rust-gdk4-0.9.6
                                       rust-gdk4-sys-0.9.6
                                       rust-generic-array-0.14.7
                                       rust-getrandom-0.2.17
                                       rust-getrandom-0.3.4
                                       rust-getrandom-0.4.3
                                       rust-gimli-0.32.3
                                       rust-gio-0.20.12
                                       rust-gio-sys-0.20.10
                                       rust-glib-0.20.12
                                       rust-glib-build-tools-0.20.0
                                       rust-glib-macros-0.20.12
                                       rust-glib-sys-0.20.10
                                       rust-globset-0.4.20
                                       rust-gobject-sys-0.20.10
                                       rust-graphene-rs-0.20.10
                                       rust-graphene-sys-0.20.10
                                       rust-gsk4-0.9.6
                                       rust-gsk4-sys-0.9.6
                                       rust-gtk4-0.9.7
                                       rust-gtk4-macros-0.9.5
                                       rust-gtk4-sys-0.9.6
                                       rust-hashbrown-0.14.5
                                       rust-hashbrown-0.17.1
                                       rust-heck-0.5.0
                                       rust-hex-0.4.3
                                       rust-hmac-0.12.1
                                       rust-human-panic-2.0.8
                                       rust-icu-collections-2.3.0
                                       rust-icu-locale-core-2.3.0
                                       rust-icu-normalizer-2.3.0
                                       rust-icu-normalizer-data-2.3.0
                                       rust-icu-properties-2.3.0
                                       rust-icu-properties-data-2.3.0
                                       rust-icu-provider-2.3.0
                                       rust-ident-case-1.0.1
                                       rust-idna-1.1.0
                                       rust-idna-adapter-1.2.2
                                       rust-ignore-0.4.33
                                       rust-indexmap-2.14.0
                                       rust-inout-0.1.4
                                       rust-intl-memoizer-0.5.3
                                       rust-intl-pluralrules-7.0.2
                                       rust-is-docker-0.2.0
                                       rust-is-wsl-0.4.0
                                       rust-is-terminal-polyfill-1.70.2
                                       rust-itoa-1.0.18
                                       rust-jobserver-0.1.35
                                       rust-js-sys-0.3.104
                                       rust-kinda-virtual-fs-0.1.1
                                       rust-lazy-static-1.5.0
                                       rust-libadwaita-0.7.2
                                       rust-libadwaita-sys-0.7.2
                                       rust-libc-0.2.189
                                       rust-libloading-0.8.9
                                       rust-linux-raw-sys-0.12.1
                                       rust-litemap-0.8.3
                                       rust-lock-api-0.4.14
                                       rust-log-0.4.33
                                       rust-lzma-rs-0.3.0
                                       rust-lzma-sys-0.1.20
                                       rust-md-5-0.10.6
                                       rust-md5-asm-0.5.2
                                       rust-memchr-2.8.3
                                       rust-memoffset-0.9.1
                                       rust-miniz-oxide-0.8.9
                                       rust-minreq-2.14.1
                                       rust-mio-1.2.2
                                       rust-nanorand-0.7.0
                                       rust-ntapi-0.4.3
                                       rust-nu-ansi-term-0.50.3
                                       rust-num-conv-0.2.2
                                       rust-objc2-0.6.4
                                       rust-objc2-app-kit-0.3.2
                                       rust-objc2-core-foundation-0.3.2
                                       rust-objc2-encode-4.1.0
                                       rust-objc2-foundation-0.3.2
                                       rust-objc2-io-kit-0.3.2
                                       rust-object-0.37.3
                                       rust-once-cell-1.21.4
                                       rust-once-cell-polyfill-1.70.2
                                       rust-open-5.4.1
                                       rust-openssl-probe-0.1.6
                                       rust-ordered-stream-0.2.0
                                       rust-pango-0.20.12
                                       rust-pango-sys-0.20.10
                                       rust-parking-2.2.1
                                       rust-pbkdf2-0.12.2
                                       rust-percent-encoding-2.3.2
                                       rust-pin-project-lite-0.2.17
                                       rust-pkg-config-0.3.34
                                       rust-pollster-0.4.0
                                       rust-potential-utf-0.1.6
                                       rust-powerfmt-0.2.0
                                       rust-ppv-lite86-0.2.21
                                       rust-proc-macro-crate-3.5.0
                                       rust-proc-macro-hack-0.5.20+deprecated
                                       rust-proc-macro2-1.0.107
                                       rust-quick-xml-0.41.0
                                       rust-quote-1.0.47
                                       rust-r-efi-5.3.0
                                       rust-r-efi-6.0.0
                                       rust-rand-0.9.5
                                       rust-rand-chacha-0.9.0
                                       rust-rand-core-0.9.5
                                       rust-raw-window-handle-0.6.2
                                       rust-regex-automata-0.4.18
                                       rust-regex-syntax-0.8.11
                                       rust-relm4-0.9.1
                                       rust-relm4-css-0.9.0
                                       rust-relm4-macros-0.9.1
                                       rust-rfd-0.15.4
                                       rust-ring-0.17.14
                                       rust-rustc-demangle-0.1.28
                                       rust-rustc-hash-1.1.0
                                       rust-rustc-hash-2.1.3
                                       rust-rustc-version-0.4.1
                                       rust-rustix-1.1.4
                                       rust-rustls-0.21.12
                                       rust-rustls-native-certs-0.6.3
                                       rust-rustls-pemfile-1.0.4
                                       rust-rustls-webpki-0.101.7
                                       rust-rustversion-1.0.23
                                       rust-same-file-1.0.6
                                       rust-schannel-0.1.29
                                       rust-scoped-tls-1.0.1
                                       rust-scopeguard-1.2.0
                                       rust-sct-0.7.1
                                       rust-security-framework-2.11.1
                                       rust-security-framework-sys-2.17.0
                                       rust-self-cell-0.10.3
                                       rust-self-cell-1.3.0
                                       rust-semver-1.0.28
                                       rust-serde-1.0.229
                                       rust-serde-core-1.0.229
                                       rust-serde-derive-1.0.229
                                       rust-serde-json-1.0.151
                                       rust-serde-repr-0.1.21
                                       rust-serde-spanned-1.1.1
                                       rust-sha1-0.10.7
                                       rust-sharded-slab-0.1.7
                                       rust-shlex-2.0.1
                                       rust-signal-hook-registry-1.4.8
                                       rust-simd-adler32-0.3.10
                                       rust-slab-0.4.12
                                       rust-smallvec-1.15.2
                                       rust-socket2-0.6.5
                                       rust-spin-0.9.9
                                       rust-stable-deref-trait-1.2.1
                                       rust-strsim-0.11.1
                                       rust-subtle-2.6.1
                                       rust-syn-2.0.119
                                       rust-syn-3.0.3
                                       rust-synstructure-0.13.2
                                       rust-sysinfo-0.35.2
                                       rust-sysinfo-0.38.4
                                       rust-system-deps-7.0.8
                                       rust-tar-0.4.46
                                       rust-target-lexicon-0.13.5
                                       rust-tempfile-3.27.0
                                       rust-thiserror-1.0.69
                                       rust-thiserror-2.0.20
                                       rust-thiserror-impl-1.0.69
                                       rust-thiserror-impl-2.0.20
                                       rust-thread-local-1.1.10
                                       rust-time-0.3.55
                                       rust-time-core-0.1.9
                                       rust-tinystr-0.8.4
                                       rust-tokio-1.53.1
                                       rust-toml-1.1.4+spec-1.1.0
                                       rust-toml-datetime-1.1.1+spec-1.1.0
                                       rust-toml-edit-0.25.13+spec-1.1.0
                                       rust-toml-parser-1.1.3+spec-1.1.0
                                       rust-toml-writer-1.1.2+spec-1.1.0
                                       rust-tracing-0.1.44
                                       rust-tracing-attributes-0.1.31
                                       rust-tracing-core-0.1.36
                                       rust-tracing-log-0.2.0
                                       rust-tracing-subscriber-0.3.23
                                       rust-type-map-0.5.1
                                       rust-typenum-1.20.1
                                       rust-uds-windows-1.2.1
                                       rust-unic-langid-0.9.6
                                       rust-unic-langid-impl-0.9.6
                                       rust-unic-langid-macros-0.9.6
                                       rust-unic-langid-macros-impl-0.9.6
                                       rust-unicode-ident-1.0.24
                                       rust-untrusted-0.9.0
                                       rust-url-2.5.8
                                       rust-urlencoding-2.1.3
                                       rust-utf8-iter-1.0.4
                                       rust-utf8parse-0.2.2
                                       rust-uuid-1.24.0
                                       rust-valuable-0.1.1
                                       rust-version-compare-0.2.1
                                       rust-version-check-0.9.5
                                       rust-walkdir-2.5.0
                                       rust-wasi-0.11.1+wasi-snapshot-preview1
                                       rust-wasip2-1.0.4+wasi-0.2.12
                                       rust-wasm-bindgen-0.2.127
                                       rust-wasm-bindgen-futures-0.4.77
                                       rust-wasm-bindgen-macro-0.2.127
                                       rust-wasm-bindgen-macro-support-0.2.127
                                       rust-wasm-bindgen-shared-0.2.127
                                       rust-wayland-backend-0.3.16
                                       rust-wayland-client-0.31.15
                                       rust-wayland-protocols-0.32.13
                                       rust-wayland-scanner-0.31.11
                                       rust-wayland-sys-0.31.11
                                       rust-web-sys-0.3.104
                                       rust-web-time-1.1.0
                                       rust-webpki-roots-0.25.4
                                       rust-whatadistro-0.1.0
                                       rust-winapi-0.3.9
                                       rust-winapi-i686-pc-windows-gnu-0.4.0
                                       rust-winapi-util-0.1.11
                                       rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                       rust-wincompatlib-0.7.7
                                       rust-windows-0.61.3
                                       rust-windows-0.62.2
                                       rust-windows-collections-0.2.0
                                       rust-windows-collections-0.3.2
                                       rust-windows-core-0.61.2
                                       rust-windows-core-0.62.2
                                       rust-windows-future-0.2.1
                                       rust-windows-future-0.3.2
                                       rust-windows-implement-0.60.2
                                       rust-windows-interface-0.59.3
                                       rust-windows-link-0.1.3
                                       rust-windows-link-0.2.1
                                       rust-windows-numerics-0.2.0
                                       rust-windows-numerics-0.3.1
                                       rust-windows-result-0.3.4
                                       rust-windows-result-0.4.1
                                       rust-windows-strings-0.4.2
                                       rust-windows-strings-0.5.1
                                       rust-windows-sys-0.52.0
                                       rust-windows-sys-0.59.0
                                       rust-windows-sys-0.60.2
                                       rust-windows-sys-0.61.2
                                       rust-windows-targets-0.52.6
                                       rust-windows-targets-0.53.5
                                       rust-windows-threading-0.1.0
                                       rust-windows-threading-0.2.1
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
                                       rust-winnow-1.0.4
                                       rust-wit-bindgen-0.57.1
                                       rust-writeable-0.6.4
                                       rust-xattr-1.6.1
                                       rust-xz-0.1.0
                                       rust-xz2-0.1.7
                                       rust-yoke-0.8.3
                                       rust-yoke-derive-0.8.2
                                       rust-zbus-5.19.0
                                       rust-zbus-macros-5.19.0
                                       rust-zbus-names-4.3.4
                                       rust-zcheapstr-1.1.0
                                       rust-zerocopy-0.8.56
                                       rust-zerocopy-derive-0.8.56
                                       rust-zerofrom-0.1.8
                                       rust-zerofrom-derive-0.1.7
                                       rust-zeroize-1.9.0
                                       rust-zeroize-derive-1.5.0
                                       rust-zerotrie-0.2.5
                                       rust-zerovec-0.11.7
                                       rust-zerovec-derive-0.11.4
                                       rust-zip-3.0.0
                                       rust-zlib-rs-0.6.7
                                       rust-zmij-1.0.23
                                       rust-zopfli-0.8.3
                                       rust-zstd-0.13.3
                                       rust-zstd-safe-7.2.4
                                       rust-zstd-sys-2.0.16+zstd.1.5.7
                                       rust-zvariant-5.14.0
                                       rust-zvariant-derive-5.14.0
                                       rust-zvariant-utils-4.0.0))
                     (sophon-lib-0.1.6 =>
                                       (list rust-aho-corasick-1.1.5
                                        rust-anstream-1.0.0
                                        rust-anstyle-1.0.14
                                        rust-anstyle-parse-1.0.0
                                        rust-anstyle-query-1.1.5
                                        rust-anstyle-wincon-3.0.11
                                        rust-anyhow-1.0.104
                                        rust-atomic-waker-1.1.2
                                        rust-atty-0.2.14
                                        rust-base64-0.22.1
                                        rust-bitflags-2.13.1
                                        rust-block-buffer-0.10.4
                                        rust-bumpalo-3.20.3
                                        rust-bytes-1.12.1
                                        rust-cc-1.4.2
                                        rust-cfg-if-1.0.4
                                        rust-cfg-aliases-0.2.2
                                        rust-chacha20-0.10.1
                                        rust-clap-4.6.6
                                        rust-clap-builder-4.6.6
                                        rust-clap-derive-4.6.4
                                        rust-clap-lex-1.1.0
                                        rust-colorchoice-1.0.5
                                        rust-console-0.16.4
                                        rust-cpufeatures-0.3.0
                                        rust-crossbeam-channel-0.5.16
                                        rust-crossbeam-utils-0.8.22
                                        rust-crypto-common-0.1.7
                                        rust-dialoguer-0.12.0
                                        rust-digest-0.10.7
                                        rust-displaydoc-0.2.7
                                        rust-doctest-file-1.1.1
                                        rust-either-1.17.0
                                        rust-encode-unicode-1.0.0
                                        rust-equivalent-1.0.2
                                        rust-errno-0.3.14
                                        rust-fastrand-2.5.0
                                        rust-find-msvc-tools-0.1.10
                                        rust-fixedbitset-0.5.7
                                        rust-fnv-1.0.7
                                        rust-foldhash-0.1.5
                                        rust-form-urlencoded-1.2.2
                                        rust-fs2-0.4.3
                                        rust-fs-extra-1.3.0
                                        rust-futures-channel-0.3.34
                                        rust-futures-core-0.3.34
                                        rust-futures-io-0.3.34
                                        rust-futures-sink-0.3.34
                                        rust-futures-task-0.3.34
                                        rust-futures-util-0.3.34
                                        rust-generator-0.8.9
                                        rust-generic-array-0.14.7
                                        rust-getrandom-0.2.17
                                        rust-getrandom-0.4.3
                                        rust-h2-0.4.15
                                        rust-hashbrown-0.15.5
                                        rust-hashbrown-0.17.1
                                        rust-heck-0.5.0
                                        rust-hermit-abi-0.1.19
                                        rust-http-1.5.0
                                        rust-http-body-1.1.0
                                        rust-http-body-util-0.1.5
                                        rust-httparse-1.10.1
                                        rust-hyper-1.11.0
                                        rust-hyper-rustls-0.27.9
                                        rust-hyper-util-0.1.20
                                        rust-icu-collections-2.3.0
                                        rust-icu-locale-core-2.3.0
                                        rust-icu-normalizer-2.3.0
                                        rust-icu-normalizer-data-2.3.0
                                        rust-icu-properties-2.3.0
                                        rust-icu-properties-data-2.3.0
                                        rust-icu-provider-2.3.0
                                        rust-idna-1.1.0
                                        rust-idna-adapter-1.2.2
                                        rust-indexmap-2.14.0
                                        rust-indicatif-0.18.6
                                        rust-interprocess-2.4.3
                                        rust-ipnet-2.12.1
                                        rust-is-terminal-polyfill-1.70.2
                                        rust-itertools-0.14.0
                                        rust-itoa-1.0.18
                                        rust-jobserver-0.1.35
                                        rust-js-sys-0.3.104
                                        rust-lazy-static-1.5.0
                                        rust-libc-0.2.189
                                        rust-linux-raw-sys-0.12.1
                                        rust-litemap-0.8.3
                                        rust-log-0.4.33
                                        rust-loom-0.7.2
                                        rust-lru-slab-0.1.2
                                        rust-matchers-0.2.0
                                        rust-md-5-0.10.6
                                        rust-memchr-2.8.3
                                        rust-mio-1.2.2
                                        rust-multimap-0.10.1
                                        rust-nu-ansi-term-0.50.3
                                        rust-once-cell-1.21.4
                                        rust-once-cell-polyfill-1.70.2
                                        rust-paimon-0.1.0.2f80905
                                        rust-percent-encoding-2.3.2
                                        rust-petgraph-0.8.3
                                        rust-pin-project-lite-0.2.17
                                        rust-pkg-config-0.3.34
                                        rust-portable-atomic-1.15.0
                                        rust-potential-utf-0.1.6
                                        rust-prettyplease-0.2.37
                                        rust-proc-macro2-1.0.107
                                        rust-prost-0.14.4
                                        rust-prost-build-0.14.4
                                        rust-prost-derive-0.14.4
                                        rust-prost-types-0.14.4
                                        rust-quinn-0.11.11
                                        rust-quinn-proto-0.11.16
                                        rust-quinn-udp-0.5.15
                                        rust-quote-1.0.47
                                        rust-r-efi-6.0.0
                                        rust-rand-0.10.2
                                        rust-rand-core-0.10.1
                                        rust-rand-pcg-0.10.2
                                        rust-recvmsg-1.0.0
                                        rust-regex-1.13.1
                                        rust-regex-automata-0.4.18
                                        rust-regex-syntax-0.8.11
                                        rust-reqwest-0.12.28
                                        rust-ring-0.17.14
                                        rust-rustc-hash-2.1.3
                                        rust-rustix-1.1.4
                                        rust-rustls-0.23.43
                                        rust-rustls-pki-types-1.15.1
                                        rust-rustls-webpki-0.103.14
                                        rust-rustversion-1.0.23
                                        rust-ryu-1.0.23
                                        rust-scoped-tls-1.0.1
                                        rust-serde-1.0.229
                                        rust-serde-core-1.0.229
                                        rust-serde-derive-1.0.229
                                        rust-serde-json-1.0.151
                                        rust-serde-urlencoded-0.7.1
                                        rust-sharded-slab-0.1.7
                                        rust-shell-words-1.1.1
                                        rust-shlex-2.0.1
                                        rust-slab-0.4.12
                                        rust-smallvec-1.15.2
                                        rust-socket2-0.6.5
                                        rust-stable-deref-trait-1.2.1
                                        rust-strsim-0.11.1
                                        rust-subtle-2.6.1
                                        rust-syn-2.0.119
                                        rust-syn-3.0.3
                                        rust-sync-wrapper-1.0.2
                                        rust-synstructure-0.13.2
                                        rust-tempfile-3.27.0
                                        rust-thiserror-2.0.20
                                        rust-thiserror-impl-2.0.20
                                        rust-thread-local-1.1.10
                                        rust-tinystr-0.8.4
                                        rust-tinyvec-1.12.0
                                        rust-tinyvec-macros-0.1.1
                                        rust-tokio-1.53.1
                                        rust-tokio-rustls-0.26.4
                                        rust-tokio-util-0.7.19
                                        rust-tower-0.5.3
                                        rust-tower-http-0.6.11
                                        rust-tower-layer-0.3.3
                                        rust-tower-service-0.3.3
                                        rust-tracing-0.1.44
                                        rust-tracing-attributes-0.1.31
                                        rust-tracing-core-0.1.36
                                        rust-tracing-log-0.2.0
                                        rust-tracing-serde-0.2.0
                                        rust-tracing-subscriber-0.3.23
                                        rust-tracing-tracy-0.11.4
                                        rust-tracy-client-0.18.4
                                        rust-tracy-client-sys-0.28.0
                                        rust-try-lock-0.2.5
                                        rust-typenum-1.20.1
                                        rust-unicode-ident-1.0.24
                                        rust-unicode-width-0.2.2
                                        rust-unit-prefix-0.5.2
                                        rust-untrusted-0.9.0
                                        rust-url-2.5.8
                                        rust-utf8-iter-1.0.4
                                        rust-utf8parse-0.2.2
                                        rust-valuable-0.1.1
                                        rust-version-check-0.9.5
                                        rust-want-0.3.1
                                        rust-wasi-0.11.1+wasi-snapshot-preview1
                                        rust-wasm-bindgen-0.2.127
                                        rust-wasm-bindgen-futures-0.4.77
                                        rust-wasm-bindgen-macro-0.2.127
                                        rust-wasm-bindgen-macro-support-0.2.127
                                        rust-wasm-bindgen-shared-0.2.127
                                        rust-web-sys-0.3.104
                                        rust-web-time-1.1.0
                                        rust-webpki-roots-1.0.9
                                        rust-widestring-1.2.1
                                        rust-winapi-0.3.9
                                        rust-winapi-i686-pc-windows-gnu-0.4.0
                                        rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                        rust-windows-link-0.2.1
                                        rust-windows-result-0.4.1
                                        rust-windows-sys-0.52.0
                                        rust-windows-sys-0.61.2
                                        rust-windows-targets-0.52.6
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-msvc-0.52.6
                                        rust-writeable-0.6.4
                                        rust-yoke-0.8.3
                                        rust-yoke-derive-0.8.2
                                        rust-zerofrom-0.1.8
                                        rust-zerofrom-derive-0.1.7
                                        rust-zeroize-1.9.0
                                        rust-zerotrie-0.2.5
                                        rust-zerovec-0.11.7
                                        rust-zerovec-derive-0.11.4
                                        rust-zmij-1.0.23
                                        rust-zstd-0.13.3
                                        rust-zstd-safe-7.2.4
                                        rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (sophon-lib-0.1.8 =>
                                       (list rust-aho-corasick-1.1.5
                                        rust-anstream-1.0.0
                                        rust-anstyle-1.0.14
                                        rust-anstyle-parse-1.0.0
                                        rust-anstyle-query-1.1.5
                                        rust-anstyle-wincon-3.0.11
                                        rust-anyhow-1.0.104
                                        rust-atomic-waker-1.1.2
                                        rust-atty-0.2.14
                                        rust-base64-0.22.1
                                        rust-bitflags-2.13.1
                                        rust-block-buffer-0.10.4
                                        rust-bumpalo-3.20.3
                                        rust-bytes-1.12.1
                                        rust-cc-1.4.2
                                        rust-cfg-if-1.0.4
                                        rust-cfg-aliases-0.2.2
                                        rust-chacha20-0.10.1
                                        rust-clap-4.6.6
                                        rust-clap-builder-4.6.6
                                        rust-clap-derive-4.6.4
                                        rust-clap-lex-1.1.0
                                        rust-colorchoice-1.0.5
                                        rust-console-0.16.4
                                        rust-cpufeatures-0.3.0
                                        rust-crossbeam-channel-0.5.16
                                        rust-crossbeam-utils-0.8.22
                                        rust-crypto-common-0.1.7
                                        rust-dialoguer-0.12.0
                                        rust-digest-0.10.7
                                        rust-displaydoc-0.2.7
                                        rust-doctest-file-1.1.1
                                        rust-either-1.17.0
                                        rust-encode-unicode-1.0.0
                                        rust-equivalent-1.0.2
                                        rust-errno-0.3.14
                                        rust-fastrand-2.5.0
                                        rust-find-msvc-tools-0.1.10
                                        rust-fixedbitset-0.5.7
                                        rust-fnv-1.0.7
                                        rust-foldhash-0.1.5
                                        rust-form-urlencoded-1.2.2
                                        rust-fs2-0.4.3
                                        rust-fs-extra-1.3.0
                                        rust-futures-channel-0.3.34
                                        rust-futures-core-0.3.34
                                        rust-futures-io-0.3.34
                                        rust-futures-sink-0.3.34
                                        rust-futures-task-0.3.34
                                        rust-futures-util-0.3.34
                                        rust-generator-0.8.9
                                        rust-generic-array-0.14.7
                                        rust-getrandom-0.2.17
                                        rust-getrandom-0.4.3
                                        rust-h2-0.4.15
                                        rust-hashbrown-0.15.5
                                        rust-hashbrown-0.17.1
                                        rust-heck-0.5.0
                                        rust-hermit-abi-0.1.19
                                        rust-http-1.5.0
                                        rust-http-body-1.1.0
                                        rust-http-body-util-0.1.5
                                        rust-httparse-1.10.1
                                        rust-hyper-1.11.0
                                        rust-hyper-rustls-0.27.9
                                        rust-hyper-util-0.1.20
                                        rust-icu-collections-2.3.0
                                        rust-icu-locale-core-2.3.0
                                        rust-icu-normalizer-2.3.0
                                        rust-icu-normalizer-data-2.3.0
                                        rust-icu-properties-2.3.0
                                        rust-icu-properties-data-2.3.0
                                        rust-icu-provider-2.3.0
                                        rust-idna-1.1.0
                                        rust-idna-adapter-1.2.2
                                        rust-indexmap-2.14.0
                                        rust-indicatif-0.18.6
                                        rust-interprocess-2.4.3
                                        rust-ipnet-2.12.1
                                        rust-is-terminal-polyfill-1.70.2
                                        rust-itertools-0.14.0
                                        rust-itoa-1.0.18
                                        rust-jobserver-0.1.35
                                        rust-js-sys-0.3.104
                                        rust-lazy-static-1.5.0
                                        rust-libc-0.2.189
                                        rust-linux-raw-sys-0.12.1
                                        rust-litemap-0.8.3
                                        rust-log-0.4.33
                                        rust-loom-0.7.2
                                        rust-lru-slab-0.1.2
                                        rust-matchers-0.2.0
                                        rust-md-5-0.10.6
                                        rust-memchr-2.8.3
                                        rust-mio-1.2.2
                                        rust-multimap-0.10.1
                                        rust-nu-ansi-term-0.50.3
                                        rust-once-cell-1.21.4
                                        rust-once-cell-polyfill-1.70.2
                                        rust-paimon-0.1.0.2f80905
                                        rust-percent-encoding-2.3.2
                                        rust-petgraph-0.8.3
                                        rust-pin-project-lite-0.2.17
                                        rust-pkg-config-0.3.34
                                        rust-portable-atomic-1.15.0
                                        rust-potential-utf-0.1.6
                                        rust-prettyplease-0.2.37
                                        rust-proc-macro2-1.0.107
                                        rust-prost-0.14.4
                                        rust-prost-build-0.14.4
                                        rust-prost-derive-0.14.4
                                        rust-prost-types-0.14.4
                                        rust-quinn-0.11.11
                                        rust-quinn-proto-0.11.16
                                        rust-quinn-udp-0.5.15
                                        rust-quote-1.0.47
                                        rust-r-efi-6.0.0
                                        rust-rand-0.10.2
                                        rust-rand-core-0.10.1
                                        rust-rand-pcg-0.10.2
                                        rust-recvmsg-1.0.0
                                        rust-regex-1.13.1
                                        rust-regex-automata-0.4.18
                                        rust-regex-syntax-0.8.11
                                        rust-reqwest-0.12.28
                                        rust-ring-0.17.14
                                        rust-rustc-hash-2.1.3
                                        rust-rustix-1.1.4
                                        rust-rustls-0.23.43
                                        rust-rustls-pki-types-1.15.1
                                        rust-rustls-webpki-0.103.14
                                        rust-rustversion-1.0.23
                                        rust-ryu-1.0.23
                                        rust-scoped-tls-1.0.1
                                        rust-serde-1.0.229
                                        rust-serde-core-1.0.229
                                        rust-serde-derive-1.0.229
                                        rust-serde-json-1.0.151
                                        rust-serde-urlencoded-0.7.1
                                        rust-sharded-slab-0.1.7
                                        rust-shell-words-1.1.1
                                        rust-shlex-2.0.1
                                        rust-slab-0.4.12
                                        rust-smallvec-1.15.2
                                        rust-socket2-0.6.5
                                        rust-stable-deref-trait-1.2.1
                                        rust-strsim-0.11.1
                                        rust-subtle-2.6.1
                                        rust-syn-2.0.119
                                        rust-syn-3.0.3
                                        rust-sync-wrapper-1.0.2
                                        rust-synstructure-0.13.2
                                        rust-tempfile-3.27.0
                                        rust-thiserror-2.0.20
                                        rust-thiserror-impl-2.0.20
                                        rust-thread-local-1.1.10
                                        rust-tinystr-0.8.4
                                        rust-tinyvec-1.12.0
                                        rust-tinyvec-macros-0.1.1
                                        rust-tokio-1.53.1
                                        rust-tokio-rustls-0.26.4
                                        rust-tokio-util-0.7.19
                                        rust-tower-0.5.3
                                        rust-tower-http-0.6.11
                                        rust-tower-layer-0.3.3
                                        rust-tower-service-0.3.3
                                        rust-tracing-0.1.44
                                        rust-tracing-attributes-0.1.31
                                        rust-tracing-core-0.1.36
                                        rust-tracing-log-0.2.0
                                        rust-tracing-serde-0.2.0
                                        rust-tracing-subscriber-0.3.23
                                        rust-tracing-tracy-0.11.4
                                        rust-tracy-client-0.18.4
                                        rust-tracy-client-sys-0.28.0
                                        rust-try-lock-0.2.5
                                        rust-typenum-1.20.1
                                        rust-unicode-ident-1.0.24
                                        rust-unicode-width-0.2.2
                                        rust-unit-prefix-0.5.2
                                        rust-untrusted-0.9.0
                                        rust-url-2.5.8
                                        rust-utf8-iter-1.0.4
                                        rust-utf8parse-0.2.2
                                        rust-valuable-0.1.1
                                        rust-version-check-0.9.5
                                        rust-want-0.3.1
                                        rust-wasi-0.11.1+wasi-snapshot-preview1
                                        rust-wasm-bindgen-0.2.127
                                        rust-wasm-bindgen-futures-0.4.77
                                        rust-wasm-bindgen-macro-0.2.127
                                        rust-wasm-bindgen-macro-support-0.2.127
                                        rust-wasm-bindgen-shared-0.2.127
                                        rust-web-sys-0.3.104
                                        rust-web-time-1.1.0
                                        rust-webpki-roots-1.0.9
                                        rust-widestring-1.2.1
                                        rust-winapi-0.3.9
                                        rust-winapi-i686-pc-windows-gnu-0.4.0
                                        rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                        rust-windows-link-0.2.1
                                        rust-windows-result-0.4.1
                                        rust-windows-sys-0.52.0
                                        rust-windows-sys-0.61.2
                                        rust-windows-targets-0.52.6
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-msvc-0.52.6
                                        rust-writeable-0.6.4
                                        rust-yoke-0.8.3
                                        rust-yoke-derive-0.8.2
                                        rust-zerofrom-0.1.8
                                        rust-zerofrom-derive-0.1.7
                                        rust-zeroize-1.9.0
                                        rust-zerotrie-0.2.5
                                        rust-zerovec-0.11.7
                                        rust-zerovec-derive-0.11.4
                                        rust-zmij-1.0.23
                                        rust-zstd-0.13.3
                                        rust-zstd-safe-7.2.4
                                        rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (sophon-lib-0.1.9 =>
                                       (list rust-aho-corasick-1.1.5
                                        rust-anstream-1.0.0
                                        rust-anstyle-1.0.14
                                        rust-anstyle-parse-1.0.0
                                        rust-anstyle-query-1.1.5
                                        rust-anstyle-wincon-3.0.11
                                        rust-anyhow-1.0.104
                                        rust-atomic-waker-1.1.2
                                        rust-atty-0.2.14
                                        rust-base64-0.22.1
                                        rust-bitflags-2.13.1
                                        rust-block-buffer-0.10.4
                                        rust-bumpalo-3.20.3
                                        rust-bytes-1.12.1
                                        rust-cc-1.4.4
                                        rust-cfg-if-1.0.4
                                        rust-cfg-aliases-0.2.2
                                        rust-chacha20-0.10.1
                                        rust-clap-4.6.6
                                        rust-clap-builder-4.6.6
                                        rust-clap-derive-4.6.4
                                        rust-clap-lex-1.1.0
                                        rust-colorchoice-1.0.5
                                        rust-console-0.16.4
                                        rust-cpufeatures-0.3.0
                                        rust-crossbeam-channel-0.5.16
                                        rust-crossbeam-utils-0.8.22
                                        rust-crypto-common-0.1.7
                                        rust-dialoguer-0.12.0
                                        rust-digest-0.10.7
                                        rust-displaydoc-0.2.7
                                        rust-doctest-file-1.1.1
                                        rust-either-1.18.0
                                        rust-encode-unicode-1.0.0
                                        rust-equivalent-1.0.2
                                        rust-errno-0.3.14
                                        rust-fastrand-2.5.0
                                        rust-find-msvc-tools-0.1.11
                                        rust-fixedbitset-0.5.7
                                        rust-fnv-1.0.7
                                        rust-foldhash-0.1.5
                                        rust-form-urlencoded-1.2.2
                                        rust-fs2-0.4.3
                                        rust-fs-extra-1.3.0
                                        rust-futures-channel-0.3.34
                                        rust-futures-core-0.3.34
                                        rust-futures-io-0.3.34
                                        rust-futures-sink-0.3.34
                                        rust-futures-task-0.3.34
                                        rust-futures-util-0.3.34
                                        rust-generator-0.8.9
                                        rust-generic-array-0.14.7
                                        rust-getrandom-0.2.17
                                        rust-getrandom-0.4.3
                                        rust-h2-0.4.19
                                        rust-hashbrown-0.15.5
                                        rust-hashbrown-0.17.1
                                        rust-heck-0.5.0
                                        rust-hermit-abi-0.1.19
                                        rust-http-1.5.0
                                        rust-http-body-1.1.0
                                        rust-http-body-util-0.1.5
                                        rust-httparse-1.10.1
                                        rust-hyper-1.11.0
                                        rust-hyper-rustls-0.27.9
                                        rust-hyper-util-0.1.20
                                        rust-icu-collections-2.3.0
                                        rust-icu-locale-core-2.3.0
                                        rust-icu-normalizer-2.3.0
                                        rust-icu-normalizer-data-2.3.0
                                        rust-icu-properties-2.3.0
                                        rust-icu-properties-data-2.3.0
                                        rust-icu-provider-2.3.1
                                        rust-idna-1.1.0
                                        rust-idna-adapter-1.2.2
                                        rust-indexmap-2.14.0
                                        rust-indicatif-0.18.6
                                        rust-interprocess-2.4.3
                                        rust-ipnet-2.12.1
                                        rust-is-terminal-polyfill-1.70.2
                                        rust-itertools-0.14.0
                                        rust-itoa-1.0.18
                                        rust-jobserver-0.1.35
                                        rust-js-sys-0.3.104
                                        rust-lazy-static-1.5.0
                                        rust-libc-0.2.189
                                        rust-linux-raw-sys-0.12.1
                                        rust-litemap-0.8.3
                                        rust-log-0.4.34
                                        rust-loom-0.7.2
                                        rust-lru-slab-0.1.2
                                        rust-matchers-0.2.0
                                        rust-md-5-0.10.6
                                        rust-memchr-2.8.3
                                        rust-mio-1.2.2
                                        rust-multimap-0.10.1
                                        rust-nu-ansi-term-0.50.3
                                        rust-once-cell-1.21.4
                                        rust-once-cell-polyfill-1.70.2
                                        rust-paimon-0.1.0.2f80905
                                        rust-percent-encoding-2.3.2
                                        rust-petgraph-0.8.3
                                        rust-pin-project-lite-0.2.17
                                        rust-pkg-config-0.3.34
                                        rust-portable-atomic-1.15.0
                                        rust-potential-utf-0.1.6
                                        rust-prettyplease-0.2.37
                                        rust-proc-macro2-1.0.107
                                        rust-prost-0.14.4
                                        rust-prost-build-0.14.4
                                        rust-prost-derive-0.14.4
                                        rust-prost-types-0.14.4
                                        rust-quinn-0.11.11
                                        rust-quinn-proto-0.11.17
                                        rust-quinn-udp-0.5.15
                                        rust-quote-1.0.47
                                        rust-r-efi-6.0.0
                                        rust-rand-0.10.2
                                        rust-rand-core-0.10.1
                                        rust-rand-pcg-0.10.2
                                        rust-recvmsg-1.0.0
                                        rust-regex-1.13.1
                                        rust-regex-automata-0.4.18
                                        rust-regex-syntax-0.8.11
                                        rust-reqwest-0.12.28
                                        rust-ring-0.17.14
                                        rust-rustc-hash-2.1.3
                                        rust-rustix-1.1.4
                                        rust-rustls-0.23.43
                                        rust-rustls-pki-types-1.15.1
                                        rust-rustls-webpki-0.103.15
                                        rust-rustversion-1.0.23
                                        rust-ryu-1.0.23
                                        rust-scoped-tls-1.0.1
                                        rust-serde-1.0.229
                                        rust-serde-core-1.0.229
                                        rust-serde-derive-1.0.229
                                        rust-serde-json-1.0.151
                                        rust-serde-urlencoded-0.7.1
                                        rust-sharded-slab-0.1.7
                                        rust-shell-words-1.1.1
                                        rust-shlex-2.0.1
                                        rust-slab-0.4.12
                                        rust-smallvec-1.15.2
                                        rust-socket2-0.6.5
                                        rust-stable-deref-trait-1.2.1
                                        rust-strsim-0.11.1
                                        rust-subtle-2.6.1
                                        rust-syn-2.0.119
                                        rust-syn-3.0.4
                                        rust-sync-wrapper-1.0.2
                                        rust-synstructure-0.13.2
                                        rust-tempfile-3.27.0
                                        rust-thiserror-2.0.20
                                        rust-thiserror-impl-2.0.20
                                        rust-thread-local-1.1.10
                                        rust-tinystr-0.8.4
                                        rust-tinyvec-1.12.0
                                        rust-tinyvec-macros-0.1.1
                                        rust-tokio-1.53.1
                                        rust-tokio-rustls-0.26.4
                                        rust-tokio-util-0.7.19
                                        rust-tower-0.5.3
                                        rust-tower-http-0.6.11
                                        rust-tower-layer-0.3.3
                                        rust-tower-service-0.3.3
                                        rust-tracing-0.1.44
                                        rust-tracing-attributes-0.1.31
                                        rust-tracing-core-0.1.36
                                        rust-tracing-log-0.2.0
                                        rust-tracing-serde-0.2.0
                                        rust-tracing-subscriber-0.3.23
                                        rust-tracing-tracy-0.11.4
                                        rust-tracy-client-0.18.4
                                        rust-tracy-client-sys-0.28.0
                                        rust-try-lock-0.2.5
                                        rust-typenum-1.20.1
                                        rust-unicode-ident-1.0.24
                                        rust-unicode-width-0.2.2
                                        rust-unit-prefix-0.5.2
                                        rust-untrusted-0.9.0
                                        rust-url-2.5.8
                                        rust-utf8-iter-1.0.4
                                        rust-utf8parse-0.2.2
                                        rust-valuable-0.1.1
                                        rust-version-check-0.9.5
                                        rust-want-0.3.1
                                        rust-wasi-0.11.1+wasi-snapshot-preview1
                                        rust-wasm-bindgen-0.2.127
                                        rust-wasm-bindgen-futures-0.4.77
                                        rust-wasm-bindgen-macro-0.2.127
                                        rust-wasm-bindgen-macro-support-0.2.127
                                        rust-wasm-bindgen-shared-0.2.127
                                        rust-web-sys-0.3.104
                                        rust-web-time-1.1.0
                                        rust-webpki-roots-1.0.9
                                        rust-widestring-1.2.1
                                        rust-winapi-0.3.9
                                        rust-winapi-i686-pc-windows-gnu-0.4.0
                                        rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                        rust-windows-link-0.2.1
                                        rust-windows-result-0.4.1
                                        rust-windows-sys-0.52.0
                                        rust-windows-sys-0.61.2
                                        rust-windows-targets-0.52.6
                                        rust-windows-aarch64-gnullvm-0.52.6
                                        rust-windows-aarch64-msvc-0.52.6
                                        rust-windows-i686-gnu-0.52.6
                                        rust-windows-i686-gnullvm-0.52.6
                                        rust-windows-i686-msvc-0.52.6
                                        rust-windows-x86-64-gnu-0.52.6
                                        rust-windows-x86-64-gnullvm-0.52.6
                                        rust-windows-x86-64-msvc-0.52.6
                                        rust-writeable-0.6.4
                                        rust-yoke-0.8.3
                                        rust-yoke-derive-0.8.2
                                        rust-zerofrom-0.1.8
                                        rust-zerofrom-derive-0.1.7
                                        rust-zeroize-1.9.0
                                        rust-zerotrie-0.2.5
                                        rust-zerovec-0.11.8
                                        rust-zerovec-derive-0.11.6
                                        rust-zmij-1.0.23
                                        rust-zstd-0.13.3
                                        rust-zstd-safe-7.2.4
                                        rust-zstd-sys-2.0.16+zstd.1.5.7))
                     (the-honkers-railway-launcher =>
                                                   (list rust-addr2line-0.25.1
                                                    rust-adler2-2.0.1
                                                    rust-aes-0.8.4
                                                    rust-ahash-0.8.12
                                                    rust-aho-corasick-1.1.5
                                                    rust-allocator-api2-0.2.21
                                                    rust-anime-game-core-1.38.10.ba60faf
                                                    rust-anime-launcher-sdk-1.35.12.4b9cb6e
                                                    rust-anstream-1.0.0
                                                    rust-anstyle-1.0.14
                                                    rust-anstyle-parse-1.0.0
                                                    rust-anstyle-query-1.1.5
                                                    rust-anstyle-wincon-3.0.11
                                                    rust-anyhow-1.0.104
                                                    rust-arbitrary-1.4.2
                                                    rust-arrayref-0.3.9
                                                    rust-arrayvec-0.7.8
                                                    rust-ashpd-0.11.1
                                                    rust-async-broadcast-0.7.2
                                                    rust-async-recursion-1.1.1
                                                    rust-async-trait-0.1.92
                                                    rust-atomic-waker-1.1.2
                                                    rust-autocfg-1.5.1
                                                    rust-backtrace-0.3.76
                                                    rust-base64-0.21.7
                                                    rust-base64-0.22.1
                                                    rust-bitflags-2.13.1
                                                    rust-blake3-1.8.6
                                                    rust-block-buffer-0.10.4
                                                    rust-block2-0.6.2
                                                    rust-bstr-1.13.1
                                                    rust-bumpalo-3.20.3
                                                    rust-byteorder-1.5.0
                                                    rust-bytes-1.12.1
                                                    rust-bzip2-0.4.4
                                                    rust-bzip2-0.5.2
                                                    rust-bzip2-sys-0.1.13+1.0.8
                                                    rust-cached-0.55.1
                                                    rust-cached-proc-macro-0.24.0
                                                    rust-cached-proc-macro-types-0.1.1
                                                    rust-cairo-rs-0.20.12
                                                    rust-cairo-sys-rs-0.20.10
                                                    rust-cc-1.4.2
                                                    rust-cfg-expr-0.20.8
                                                    rust-cfg-if-1.0.4
                                                    rust-cfg-aliases-0.2.2
                                                    rust-chacha20-0.10.1
                                                    rust-cipher-0.4.4
                                                    rust-colorchoice-1.0.5
                                                    rust-constant-time-eq-0.3.1
                                                    rust-constant-time-eq-0.4.2
                                                    rust-core-foundation-0.9.4
                                                    rust-core-foundation-sys-0.8.7
                                                    rust-cpufeatures-0.2.17
                                                    rust-cpufeatures-0.3.0
                                                    rust-crc-3.4.0
                                                    rust-crc-catalog-2.5.0
                                                    rust-crc32fast-1.5.0
                                                    rust-crossbeam-channel-0.5.16
                                                    rust-crossbeam-deque-0.8.7
                                                    rust-crossbeam-epoch-0.9.20
                                                    rust-crossbeam-utils-0.8.22
                                                    rust-crypto-common-0.1.7
                                                    rust-darling-0.20.11
                                                    rust-darling-core-0.20.11
                                                    rust-darling-macro-0.20.11
                                                    rust-deflate64-0.1.12
                                                    rust-deranged-0.5.8
                                                    rust-derive-arbitrary-1.4.2
                                                    rust-digest-0.10.7
                                                    rust-dispatch2-0.3.1
                                                    rust-displaydoc-0.2.7
                                                    rust-dlib-0.5.3
                                                    rust-dns-lookup-2.1.1
                                                    rust-doctest-file-1.1.1
                                                    rust-downcast-rs-1.2.1
                                                    rust-either-1.17.0
                                                    rust-endi-1.1.1
                                                    rust-enum-ordinalize-4.4.2
                                                    rust-enum-ordinalize-derive-4.4.2
                                                    rust-enumflags2-0.7.12
                                                    rust-enumflags2-derive-0.7.12
                                                    rust-equivalent-1.0.2
                                                    rust-errno-0.3.14
                                                    rust-event-listener-5.4.2
                                                    rust-event-listener-strategy-0.5.4
                                                    rust-fastrand-2.5.0
                                                    rust-field-offset-0.3.6
                                                    rust-filetime-0.2.29
                                                    rust-find-msvc-tools-0.1.10
                                                    rust-fixedbitset-0.5.7
                                                    rust-flate2-1.1.9
                                                    rust-fluent-bundle-0.16.0
                                                    rust-fluent-langneg-0.13.1
                                                    rust-fluent-syntax-0.12.0
                                                    rust-fluent-template-macros-0.13.3
                                                    rust-fluent-templates-0.13.3
                                                    rust-flume-0.11.1
                                                    rust-fnv-1.0.7
                                                    rust-foldhash-0.1.5
                                                    rust-form-urlencoded-1.2.2
                                                    rust-fragile-2.1.0
                                                    rust-fs2-0.4.3
                                                    rust-fs-extra-1.3.0
                                                    rust-futures-0.3.34
                                                    rust-futures-channel-0.3.34
                                                    rust-futures-core-0.3.34
                                                    rust-futures-executor-0.3.34
                                                    rust-futures-io-0.3.34
                                                    rust-futures-lite-2.6.1
                                                    rust-futures-macro-0.3.34
                                                    rust-futures-sink-0.3.34
                                                    rust-futures-task-0.3.34
                                                    rust-futures-util-0.3.34
                                                    rust-gdk-pixbuf-0.20.10
                                                    rust-gdk-pixbuf-sys-0.20.10
                                                    rust-gdk4-0.9.6
                                                    rust-gdk4-sys-0.9.6
                                                    rust-generic-array-0.14.7
                                                    rust-getrandom-0.2.17
                                                    rust-getrandom-0.3.4
                                                    rust-getrandom-0.4.3
                                                    rust-gimli-0.32.3
                                                    rust-gio-0.20.12
                                                    rust-gio-sys-0.20.10
                                                    rust-glib-0.20.12
                                                    rust-glib-build-tools-0.20.0
                                                    rust-glib-macros-0.20.12
                                                    rust-glib-sys-0.20.10
                                                    rust-globset-0.4.20
                                                    rust-gobject-sys-0.20.10
                                                    rust-graphene-rs-0.20.10
                                                    rust-graphene-sys-0.20.10
                                                    rust-gsk4-0.9.6
                                                    rust-gsk4-sys-0.9.6
                                                    rust-gtk4-0.9.7
                                                    rust-gtk4-macros-0.9.5
                                                    rust-gtk4-sys-0.9.6
                                                    rust-h2-0.4.15
                                                    rust-hashbrown-0.14.5
                                                    rust-hashbrown-0.15.5
                                                    rust-hashbrown-0.17.1
                                                    rust-heck-0.5.0
                                                    rust-hex-0.4.3
                                                    rust-hmac-0.12.1
                                                    rust-http-1.5.0
                                                    rust-http-body-1.1.0
                                                    rust-http-body-util-0.1.5
                                                    rust-httparse-1.10.1
                                                    rust-human-panic-2.0.8
                                                    rust-hyper-1.11.0
                                                    rust-hyper-rustls-0.27.9
                                                    rust-hyper-util-0.1.20
                                                    rust-icu-collections-2.3.0
                                                    rust-icu-locale-core-2.3.0
                                                    rust-icu-normalizer-2.3.0
                                                    rust-icu-normalizer-data-2.3.0
                                                    rust-icu-properties-2.3.0
                                                    rust-icu-properties-data-2.3.0
                                                    rust-icu-provider-2.3.0
                                                    rust-ident-case-1.0.1
                                                    rust-idna-1.1.0
                                                    rust-idna-adapter-1.2.2
                                                    rust-ignore-0.4.33
                                                    rust-indexmap-2.14.0
                                                    rust-inout-0.1.4
                                                    rust-interprocess-2.4.3
                                                    rust-intl-memoizer-0.5.3
                                                    rust-intl-pluralrules-7.0.2
                                                    rust-ipnet-2.12.1
                                                    rust-is-docker-0.2.0
                                                    rust-is-wsl-0.4.0
                                                    rust-is-terminal-polyfill-1.70.2
                                                    rust-itertools-0.14.0
                                                    rust-itoa-1.0.18
                                                    rust-jobserver-0.1.35
                                                    rust-js-sys-0.3.104
                                                    rust-kinda-virtual-fs-0.1.1
                                                    rust-lazy-static-1.5.0
                                                    rust-libadwaita-0.7.2
                                                    rust-libadwaita-sys-0.7.2
                                                    rust-libc-0.2.189
                                                    rust-libloading-0.8.9
                                                    rust-linux-raw-sys-0.12.1
                                                    rust-litemap-0.8.3
                                                    rust-lock-api-0.4.14
                                                    rust-log-0.4.33
                                                    rust-lru-slab-0.1.2
                                                    rust-lzma-rs-0.3.0
                                                    rust-lzma-sys-0.1.20
                                                    rust-md-5-0.10.6
                                                    rust-md5-asm-0.5.2
                                                    rust-memchr-2.8.3
                                                    rust-memoffset-0.9.1
                                                    rust-miniz-oxide-0.8.9
                                                    rust-minreq-2.14.1
                                                    rust-mio-1.2.2
                                                    rust-multimap-0.10.1
                                                    rust-nanorand-0.7.0
                                                    rust-ntapi-0.4.3
                                                    rust-nu-ansi-term-0.50.3
                                                    rust-num-conv-0.2.2
                                                    rust-objc2-0.6.4
                                                    rust-objc2-app-kit-0.3.2
                                                    rust-objc2-core-foundation-0.3.2
                                                    rust-objc2-encode-4.1.0
                                                    rust-objc2-foundation-0.3.2
                                                    rust-objc2-io-kit-0.3.2
                                                    rust-object-0.37.3
                                                    rust-once-cell-1.21.4
                                                    rust-once-cell-polyfill-1.70.2
                                                    rust-open-5.4.1
                                                    rust-openssl-probe-0.1.6
                                                    rust-ordered-stream-0.2.0
                                                    rust-pango-0.20.12
                                                    rust-pango-sys-0.20.10
                                                    rust-parking-2.2.1
                                                    rust-pbkdf2-0.12.2
                                                    rust-percent-encoding-2.3.2
                                                    rust-petgraph-0.8.3
                                                    rust-pin-project-lite-0.2.17
                                                    rust-pkg-config-0.3.34
                                                    rust-pollster-0.4.0
                                                    rust-potential-utf-0.1.6
                                                    rust-powerfmt-0.2.0
                                                    rust-ppv-lite86-0.2.21
                                                    rust-prettyplease-0.2.37
                                                    rust-proc-macro-crate-3.5.0
                                                    rust-proc-macro-hack-0.5.20+deprecated
                                                    rust-proc-macro2-1.0.107
                                                    rust-prost-0.14.4
                                                    rust-prost-build-0.14.4
                                                    rust-prost-derive-0.14.4
                                                    rust-prost-types-0.14.4
                                                    rust-quick-xml-0.41.0
                                                    rust-quinn-0.11.11
                                                    rust-quinn-proto-0.11.16
                                                    rust-quinn-udp-0.5.15
                                                    rust-quote-1.0.47
                                                    rust-r-efi-5.3.0
                                                    rust-r-efi-6.0.0
                                                    rust-rand-0.9.5
                                                    rust-rand-0.10.2
                                                    rust-rand-chacha-0.9.0
                                                    rust-rand-core-0.9.5
                                                    rust-rand-core-0.10.1
                                                    rust-rand-pcg-0.10.2
                                                    rust-raw-window-handle-0.6.2
                                                    rust-recvmsg-1.0.0
                                                    rust-regex-1.13.1
                                                    rust-regex-automata-0.4.18
                                                    rust-regex-syntax-0.8.11
                                                    rust-relm4-0.9.1
                                                    rust-relm4-css-0.9.0
                                                    rust-relm4-macros-0.9.1
                                                    rust-reqwest-0.12.28
                                                    rust-rfd-0.15.4
                                                    rust-ring-0.17.14
                                                    rust-rustc-demangle-0.1.28
                                                    rust-rustc-hash-2.1.3
                                                    rust-rustc-version-0.4.1
                                                    rust-rustix-1.1.4
                                                    rust-rustls-0.21.12
                                                    rust-rustls-0.23.43
                                                    rust-rustls-native-certs-0.6.3
                                                    rust-rustls-pemfile-1.0.4
                                                    rust-rustls-pki-types-1.15.1
                                                    rust-rustls-webpki-0.101.7
                                                    rust-rustls-webpki-0.103.14
                                                    rust-rustversion-1.0.23
                                                    rust-ryu-1.0.23
                                                    rust-same-file-1.0.6
                                                    rust-schannel-0.1.29
                                                    rust-scoped-tls-1.0.1
                                                    rust-scopeguard-1.2.0
                                                    rust-sct-0.7.1
                                                    rust-security-framework-2.11.1
                                                    rust-security-framework-sys-2.17.0
                                                    rust-self-cell-1.3.0
                                                    rust-semver-1.0.28
                                                    rust-serde-1.0.229
                                                    rust-serde-core-1.0.229
                                                    rust-serde-derive-1.0.229
                                                    rust-serde-json-1.0.151
                                                    rust-serde-repr-0.1.21
                                                    rust-serde-spanned-1.1.1
                                                    rust-serde-urlencoded-0.7.1
                                                    rust-sha1-0.10.7
                                                    rust-sharded-slab-0.1.7
                                                    rust-shlex-2.0.1
                                                    rust-signal-hook-registry-1.4.8
                                                    rust-simd-adler32-0.3.10
                                                    rust-slab-0.4.12
                                                    rust-smallvec-1.15.2
                                                    rust-socket2-0.6.5
                                                    rust-sophon-lib-0.1.8.58d223a
                                                    rust-spin-0.9.9
                                                    rust-stable-deref-trait-1.2.1
                                                    rust-strsim-0.11.1
                                                    rust-subtle-2.6.1
                                                    rust-syn-2.0.119
                                                    rust-syn-3.0.3
                                                    rust-sync-wrapper-1.0.2
                                                    rust-synstructure-0.13.2
                                                    rust-sysinfo-0.35.2
                                                    rust-sysinfo-0.38.4
                                                    rust-system-deps-7.0.8
                                                    rust-tar-0.4.46
                                                    rust-target-lexicon-0.13.5
                                                    rust-tempfile-3.27.0
                                                    rust-thiserror-1.0.69
                                                    rust-thiserror-2.0.20
                                                    rust-thiserror-impl-1.0.69
                                                    rust-thiserror-impl-2.0.20
                                                    rust-thread-local-1.1.10
                                                    rust-time-0.3.55
                                                    rust-time-core-0.1.9
                                                    rust-tinystr-0.8.4
                                                    rust-tinyvec-1.12.0
                                                    rust-tinyvec-macros-0.1.1
                                                    rust-tokio-1.53.1
                                                    rust-tokio-rustls-0.26.4
                                                    rust-tokio-util-0.7.19
                                                    rust-toml-1.1.4+spec-1.1.0
                                                    rust-toml-datetime-1.1.1+spec-1.1.0
                                                    rust-toml-edit-0.25.13+spec-1.1.0
                                                    rust-toml-parser-1.1.3+spec-1.1.0
                                                    rust-toml-writer-1.1.2+spec-1.1.0
                                                    rust-tower-0.5.3
                                                    rust-tower-http-0.6.11
                                                    rust-tower-layer-0.3.3
                                                    rust-tower-service-0.3.3
                                                    rust-tracing-0.1.44
                                                    rust-tracing-attributes-0.1.31
                                                    rust-tracing-core-0.1.36
                                                    rust-tracing-log-0.2.0
                                                    rust-tracing-subscriber-0.3.23
                                                    rust-try-lock-0.2.5
                                                    rust-type-map-0.5.1
                                                    rust-typenum-1.20.1
                                                    rust-uds-windows-1.2.1
                                                    rust-unic-langid-0.9.6
                                                    rust-unic-langid-impl-0.9.6
                                                    rust-unic-langid-macros-0.9.6
                                                    rust-unic-langid-macros-impl-0.9.6
                                                    rust-unicode-ident-1.0.24
                                                    rust-untrusted-0.9.0
                                                    rust-url-2.5.8
                                                    rust-urlencoding-2.1.3
                                                    rust-utf8-iter-1.0.4
                                                    rust-utf8parse-0.2.2
                                                    rust-uuid-1.24.0
                                                    rust-valuable-0.1.1
                                                    rust-version-compare-0.2.1
                                                    rust-version-check-0.9.5
                                                    rust-walkdir-2.5.0
                                                    rust-want-0.3.1
                                                    rust-wasi-0.11.1+wasi-snapshot-preview1
                                                    rust-wasip2-1.0.4+wasi-0.2.12
                                                    rust-wasm-bindgen-0.2.127
                                                    rust-wasm-bindgen-futures-0.4.77
                                                    rust-wasm-bindgen-macro-0.2.127
                                                    rust-wasm-bindgen-macro-support-0.2.127
                                                    rust-wasm-bindgen-shared-0.2.127
                                                    rust-wayland-backend-0.3.16
                                                    rust-wayland-client-0.31.15
                                                    rust-wayland-protocols-0.32.13
                                                    rust-wayland-scanner-0.31.11
                                                    rust-wayland-sys-0.31.11
                                                    rust-web-sys-0.3.104
                                                    rust-web-time-1.1.0
                                                    rust-webpki-roots-0.25.4
                                                    rust-webpki-roots-1.0.9
                                                    rust-whatadistro-0.1.0
                                                    rust-widestring-1.2.1
                                                    rust-winapi-0.3.9
                                                    rust-winapi-i686-pc-windows-gnu-0.4.0
                                                    rust-winapi-util-0.1.11
                                                    rust-winapi-x86-64-pc-windows-gnu-0.4.0
                                                    rust-wincompatlib-0.7.7
                                                    rust-windows-0.61.3
                                                    rust-windows-0.62.2
                                                    rust-windows-collections-0.2.0
                                                    rust-windows-collections-0.3.2
                                                    rust-windows-core-0.61.2
                                                    rust-windows-core-0.62.2
                                                    rust-windows-future-0.2.1
                                                    rust-windows-future-0.3.2
                                                    rust-windows-implement-0.60.2
                                                    rust-windows-interface-0.59.3
                                                    rust-windows-link-0.1.3
                                                    rust-windows-link-0.2.1
                                                    rust-windows-numerics-0.2.0
                                                    rust-windows-numerics-0.3.1
                                                    rust-windows-result-0.3.4
                                                    rust-windows-result-0.4.1
                                                    rust-windows-strings-0.4.2
                                                    rust-windows-strings-0.5.1
                                                    rust-windows-sys-0.52.0
                                                    rust-windows-sys-0.59.0
                                                    rust-windows-sys-0.60.2
                                                    rust-windows-sys-0.61.2
                                                    rust-windows-targets-0.52.6
                                                    rust-windows-targets-0.53.5
                                                    rust-windows-threading-0.1.0
                                                    rust-windows-threading-0.2.1
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
                                                    rust-winnow-1.0.4
                                                    rust-wit-bindgen-0.57.1
                                                    rust-writeable-0.6.4
                                                    rust-xattr-1.6.1
                                                    rust-xz-0.1.0
                                                    rust-xz2-0.1.7
                                                    rust-yoke-0.8.3
                                                    rust-yoke-derive-0.8.2
                                                    rust-zbus-5.19.0
                                                    rust-zbus-macros-5.19.0
                                                    rust-zbus-names-4.3.4
                                                    rust-zcheapstr-1.1.0
                                                    rust-zerocopy-0.8.56
                                                    rust-zerocopy-derive-0.8.56
                                                    rust-zerofrom-0.1.8
                                                    rust-zerofrom-derive-0.1.7
                                                    rust-zeroize-1.9.0
                                                    rust-zeroize-derive-1.5.0
                                                    rust-zerotrie-0.2.5
                                                    rust-zerovec-0.11.7
                                                    rust-zerovec-derive-0.11.4
                                                    rust-zip-3.0.0
                                                    rust-zlib-rs-0.6.7
                                                    rust-zmij-1.0.23
                                                    rust-zopfli-0.8.3
                                                    rust-zstd-0.13.3
                                                    rust-zstd-safe-7.2.4
                                                    rust-zstd-sys-2.0.16+zstd.1.5.7
                                                    rust-zvariant-5.14.0
                                                    rust-zvariant-derive-5.14.0
                                                    rust-zvariant-utils-4.0.0)))
