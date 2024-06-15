# based on https://dev.to/cloudx/testing-our-package-build-in-the-docker-world-34p0

FROM archlinux

# binutils is for `strip` even though we're not using it
RUN pacman -Sy --noconfirm sudo fakeroot binutils

WORKDIR /build

ARG ytaudio_path=.stack-work/install/x86_64-linux-*/*/*/bin

COPY PKGBUILD ${ytaudio_path}/ytaudio ./
ADD systemd ytaudio/systemd

RUN useradd builduser \
    && passwd -d builduser \
    && (printf 'builduser ALL=(ALL) ALL\n' | tee -a /etc/sudoers) \
    && chown -R builduser /build \
    && sudo -u builduser bash -c 'makepkg -si --noconfirm'

CMD ["bash"]
