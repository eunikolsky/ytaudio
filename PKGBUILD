# Maintainer: Eugene Nikolsky <e@egeek.me>
pkgname=ytaudio
pkgver=0.3.8
pkgrel=1
epoch=
pkgdesc="Allows to download audio podcasts from youtube channels via RSS"
arch=('x86_64')
url="https://alice.egeek.me/"
license=('MIT')
groups=()
depends=(glibc gmp yt-dlp ffmpeg)
makedepends=()
checkdepends=()
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=
source=($pkgname.tar)
noextract=()
md5sums=('SKIP')
validpgpkeys=()
strip=()

package() {
  install -Dm 755 -t "$pkgdir"/usr/bin ytaudio

  find systemd -maxdepth 1 -type f -name '*.service' \
    -exec install -Dm 644 -t "$pkgdir"/usr/lib/systemd/system "{}" +
}
