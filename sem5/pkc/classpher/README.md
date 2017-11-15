# Classpher

<table>
<tr>
<td>
Classpher is a GUI application that demonstrates the functionality of multiple <a href="https://en.wikipedia.org/wiki/Classical_cipher" target="_blank">classical ciphers</a>.
</td>
</tr>
<tr><td>
This is what it looks like:<br><br>
<img src="https://i.imgur.com/uE6sOz6.png" height="75%" width="75%">
</td>
</tr>
</table>

## Getting started

### Requirements

Make sure you have the following installed:
* [Bazel](https://bazel.build/)
* [QT](https://www.qt.io/)
* [NTL](http://www.shoup.net/ntl/)

Most linux distributions should have these packages in their repositories:

#### Arch

```sh
$ pacman -S bazel qt ntl
```

#### Ubuntu

First, follow [these steps](https://docs.bazel.build/versions/master/install-ubuntu.html#install-on-ubuntu) for installing Bazel. Then:

```sh
$ apt-get install libntl27 qt5-default
```

### Running

```sh
$ bazel run :main
```

## Features

### Real time encoding

Classpher encodes and decodes user input as it is typed.

![Real time](https://i.imgur.com/pwtsMcR.gif)

### Ciphers

There are six supported ciphers:
* [Affine](https://en.wikipedia.org/wiki/Affine_cipher)
* [Caesar](https://en.wikipedia.org/wiki/Caesar_cipher)
* [Hill](https://en.wikipedia.org/wiki/Hill_cipher)
* [Permutation (Transposition)](https://en.wikipedia.org/wiki/Transposition_cipher)
* [Substitution](https://en.wikipedia.org/wiki/Substitution_cipher)
* [Vigenère (Bellaso)](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)

![Classpher ciphers](https://i.imgur.com/IGHXP7z.gif)

