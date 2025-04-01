---
title: what i use
tags: hacking, technology
description: a list of the software that i rely on
published: 12022-03-21
---

Below is a nearly-comprehensive list
of the software that I regularly use,
including under-the-hood stuff
which tends to go under-appreciated.
[^mytton]

Everything listed is [Free and Open-Source].

[Free and Open-Source]: https://en.wikipedia.org/wiki/Free_software

## system

* OS: [Linux]
* package manager: [pacman]
* service manager: [systemd]
* C library: [glibc]
* filesystem: [ext4]
* shell: [zsh], [bash]

[Linux]: https://kernel.org/
[pacman]: https://archlinux.org/pacman/
[systemd]: https://systemd.io/
[glibc]: https://gnu.org/software/libc/
[ext4]: https://kernel.org/doc/html/latest/admin-guide/ext4.html
[zsh]: https://zsh.org/
[bash]: https://gnu.org/software/bash/bash.html

## graphics

* display server: [X.Org]
* window manager: [dwm]
* program launcher: [dmenu]
* status bar: [danwmstatus]
* terminal emulator: [rxvt-unicode], [Tilda]
* GUI menus: [zenity]
* screenshots: [scrot]
* clipboard: [autocutsel], [xclip]

[X.Org]: https://x.org
[dwm]: https://dwm.suckless.org/
[dmenu]: https://tools.suckless.org/dmenu/
[danwmstatus]: https://gitlab.com/danso/dwmstatus/
[rxvt-unicode]: http://software.schmorp.de/pkg/rxvt-unicode.html
[Tilda]: https://github.com/lanoxx/tilda
[zenity]: https://gitlab.gnome.org/GNOME/zenity
[scrot]: https://github.com/resurrecting-open-source-projects/scrot
[autocutsel]: https://github.com/sigmike/autocutsel
[xclip]: https://github.com/astrand/xclip

## files

* backup: [GNU tar]
* compression: [Zstandard], [GNU Gzip], [bzip2]
* version control: [git]
* password management: [pass]
* searching: [GNU findutils], [ack]
* identification: [file]
* renaming: [detox]
* various: [procps-ng]

[GNU Tar]: https://gnu.org/software/tar/
[Zstandard]: https://facebook.github.io/zstd/
[GNU Gzip]: https://www.gnu.org/software/gzip/
[bzip2]: https://sourceware.org/bzip2/
[git]: https://git-scm.com/
[pass]: https://passwordstore.org/
[GNU findutils]: https://www.gnu.org/software/findutils/
[ack]: https://beyondgrep.com/
[file]: https://www.darwinsys.com/file/
[detox]: https://github.com/dharple/detox
[procps-ng]: https://gitlab.com/procps-ng/procps

## web

* browser: [Firefox], [Chromium]
* browser extensions:
    - performance: [NoScript]
    - privacy: [RandomUA],
               [Decentraleyes],
               [Privacy Badger],
               [HTTPS Everywhere]
    - advertisements: [uBlock Origin],
                      [SponsorBlock]
    - convenience: [Redirector],
                   [Dark Reader]

[Firefox]: https://mozilla.org/en-US/firefox/
[Chromium]: https://chromium.org/Home/
[NoScript]: https://noscript.net/
[RandomUA]: https://nora.codes/randomua/
[Decentraleyes]: https://decentraleyes.org/
[Privacy Badger]: https://privacybadger.org/
[HTTPS Everywhere]: https://eff.org/https-everywhere
[uBlock Origin]: https://github.com/gorhill/uBlock
[SponsorBlock]: https://sponsor.ajay.app/
[Redirector]: https://einaregilsson.com/redirector/
[Dark Reader]: https://darkreader.org/

## networking

* DHCP: dhclient (from [ISC DHCP])
* DNS resolver: [Unbound]
* file transfers: [openssh], [curl], [magic-wormhole]
* encryption: [GnuPG], [OpenSSL]
* mail: [offlineimap], [mutt]
* irc: [WeeChat], [ZNC]
* feed aggregator: [newsboat]
* BitTorrent: [Transmission]
* videos: [yt-dlp]
* VOIP and video calling: [Jitsi Meet], [uTox], [Signal Desktop]
* debugging: [iputils]

[ISC DHCP]: https://www.isc.org/dhcp/
[Unbound]: https://nlnetlabs.nl/projects/unbound/about/
[openssh]: https://openssh.com/
[curl]: https://curl.se/
[magic-wormhole]: https://github.com/magic-wormhole/magic-wormhole
[GnuPG]: https://gnupg.org/
[OpenSSL]: https://openssl.org/
[offlineimap]: https://www.offlineimap.org/
[mutt]: http://mutt.org/
[WeeChat]: https://weechat.org/
[ZNC]: https://wiki.znc.in/ZNC
[newsboat]: https://newsboat.org/
[Transmission]: https://transmissionbt.com/
[yt-dlp]: https://github.com/yt-dlp/yt-dlp
[Jitsi Meet]: https://jitsi.org/jitsi-meet/
[uTox]: https://github.com/uTox/uTox
[Signal Desktop]: https://signal.org/
[iputils]: https://github.com/iputils/iputils

## text

* editor: [vim]
* compilers: [GHC], [GCC], [Clang], [Typescript]
* interpreters: [Python], [dash]
* build system: [GNU Make]

[vim]: https://www.vim.org/
[GHC]: https://haskell.org/ghc/
[GCC]: https://gcc.gnu.org/
[Clang]: https://clang.llvm.org/
[Typescript]: https://typescriptlang.org/
[Python]: https://python.org/
[dash]: http://gondor.apana.org.au/~herbert/dash/
[GNU Make]: https://gnu.org/software/make/

## audio/video

* audio: [PipeWire]
* music: [mpd]
* video: [mplayer]
* image viewer: [feh], [sxiv]
* audio recording: [FFmpeg]
* codecs: [Ogg Vorbis], [Opus], [FLAC], [Xvid], [Theora], [speex]

[PipeWire]: https://pipewire.org/
[mpd]: https://musicpd.org/
[mplayer]: http://mplayerhq.hu/design7/news.html
[feh]: https://feh.finalrewind.org/
[sxiv]: https://github.com/muennich/sxiv
[FFmpeg]: https://ffmpeg.org/
[Ogg Vorbis]: https://www.xiph.org/vorbis/
[Opus]: https://www.opus-codec.org/
[FLAC]: https://xiph.org/flac/download.html
[Xvid]: https://www.xvid.com/
[Theora]: https://www.theora.org/
[speex]: https://speex.org/

## documents

* PDF reader: [MuPDF], [Evince]
* document preparation: [TeX Live], [Pandoc]
* document editing: [AbiWord]

[MuPDF]: https://mupdf.com/
[Evince]: https://wiki.gnome.org/Apps/Evince
[TeX Live]: https://tug.org/texlive/
[Pandoc]: https://pandoc.org/
[AbiWord]: https://abisource.com/

## blogging

* site generator: [Hakyll]
* HTTP server: [nginx]
* certificate renewal: [Certbot]

[Hakyll]: https://jaspervdj.be/hakyll/
[nginx]: https://nginx.org/
[Certbot]: https://certbot.eff.org/

## miscellaneous

* terminal multiplexer: [GNU screen]
* fixer: [thefuck]
* calendar: cal (from [util-linux])
* internationalization: [GNU gettext]

[GNU Screen]: https://gnu.org/software/screen/
[thefuck]: https://github.com/nvbn/thefuck
[util-linux]: https://git.kernel.org/pub/scm/utils/util-linux/util-linux.git
[GNU gettext]: https://www.gnu.org/software/gettext/

[//]: # (footnotes)

[^mytton]: With thanks to [David Mytton](https://davidmytton.blog/iuse/)
           for the idea,
           although I don't believe he did it first.
